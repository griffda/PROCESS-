CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
C                                                                     C
C  Source Code Control System                                         C
C                                                                     S
C  Information header for the PROCESS systems code modules            C
C                                                                     C
C  P.J.Knight 22 May 1992                                             S
C                                                                     C
C                                                                     C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
C
C  Module         : $Id: input.f,v 3.57 2006/05/25 09:27:02 pknight Exp pknight $
C
C  Module name    : $RCSfile: input.f,v $
C  Version no.    : $Revision: 3.57 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE INPUT

c  Input data from files.
c  This module has had its namelist calls replaced by an ANSI standard
c  Fortran 77 set of routines that still allow the same flexibility of
c  input as the original namelist routines.

      INCLUDE 'param.h'
      INCLUDE 'numer.h'

      INTEGER icode,ifunit

      EXTERNAL readnl

c  Call the replacement routine

      ifunit = nin
      call readnl(ifunit,icode)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE EDIT1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to print out the active iteration variables and
C  constraint equations for the run.
C  This used to be an ENTRY point into routine INPUT.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'labels.h'

C  Local variables
      INTEGER ii

C  External routines
      EXTERNAL oblnkl,ocmmnt

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      call oblnkl(nout)
      call ocmmnt(nout,'The following variables will be adjusted by')
      call ocmmnt(nout,'the code during the iteration process :')
      call oblnkl(nout)

      write(nout,10)
 10   format(t10,'ixc',t18,'label')

      call oblnkl(nout)

      write(nout,20) (ii,ixc(ii),lablxc(ixc(ii)),ii=1,nvar)
 20   format(t1,i3,t10,i3,t18,a8)

      call oblnkl(nout)
      call ocmmnt(nout,
     +     'The following constraint equations have been imposed,')
      if (ioptimz.eq.-1) then
         call ocmmnt(nout,
     +        'but limits will not be enforced by the code :')
      else
         call ocmmnt(nout,'and will be enforced by the code :')
      end if
      call oblnkl(nout)

      write(nout,30)
 30   format(t10,'icc',t25,'label')

      call oblnkl(nout)

      write(nout,40) (ii,icc(ii),lablcc(icc(ii)), ii=1,neqns)
 40   format(t1,i3,t10,i3,t18,a34)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE READNL(ifunit,icode)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.200
C
C--Description
C  Routine to read a NAMELIST data file without using FORTRAN NAMELIST
C  
C  Read a line string with a $ (dollar) sign and decode the
C  variables one by one until you find a line starting $END.
C  For example:
C     $INPT1
C     FTOL = 1.0E-4
C     $END
C  Arrays can be explicitly subscripted:
C     BOUNDL(3) = 1.5
C  or unsubscripted (start at 1 and work up - beware of 2D arrays)
C     SWEEP = 1.4, 1.5, 1.6, 1.75, 2.0, 2.5
C
C  The following code should be inserted into the relevant RDNLnn
C  routine if a new input variable is to be added.
C
C  For a REAL scalar variable:
C
C      else if (varnam(1:varlen).eq.'XXXXXX') then
C         if (isub1.ne.9999) goto 910
C         oldval = xxxxxx
C         call getrv(nin,line,length,iptr,lineno,xxxxxx,icode)
C         if (icode.ne.0) goto 900
C         call ranger(nout,'XXXXXX',xxxxxx,xxxmin,xxxmax)
C         if (xxxxxx.ne.oldval) then
C            clabel = 'Description of XXXXXX,'
C            write(nout,*) clabel,varnam(1:8),' = ',xxxxxx
C         end if
C
C  For an INTEGER scalar variable:
C
C      if (varnam(1:varlen).eq.'IXXXXX') then
C         if (isub1.ne.9999) goto 910
C         ioldvl = ixxxxx
C         call getiv(nin,line,length,iptr,lineno,ixxxxx,icode)
C         if (icode.ne.0) goto 900
C         call rangei(nout,'IXXXXX',ixxxxx,ixxmin,ixxmax)
C         if (ixxxxx.ne.ioldvl) then
C            clabel = 'Description of IXXXXX,'
C            write(nout,*) clabel,varnam(1:8),' = ',ixxxxx
C         end if
C
C  For a REAL array:
C
C      else if (varnam(1:varlen).eq.'XXXXXX') then
C         if (isub1.ne.9999) then
Cc           here, there was a subscript
C            call getrv(nin,line,length,iptr,lineno,rval,icode)
C            if (icode.ne.0) goto 900
C            call ranger(nout,'XXXXXX',rval,-1.0D0,1.0D9)
C            xxxxxx(isub1) = rval
C         else
Cc           here, there was no subscript.
Cc           read values until next variable or $end
C            isub1 = 1
C XYZ        continue
C            call getrv(nin,line,length,iptr,lineno,rval,icode)
C            if (icode.eq.-1) goto 20
C            if (icode.ne.0) goto 900
C            call ranger(nout,'XXXXXX',rval,xxxmin,xxxmax)
C            xxxxxx(isub1) = rval
C            isub1 = isub1 + 1
C            goto XYZ
C         end if
C
C  For an INTEGER array:
C
C      else if (varnam(1:varlen).eq.'IXXXXX') then
C         if (isub1.ne.9999) then
Cc           here, there was a subscript
C            call getiv(nin,line,length,iptr,lineno,ival,icode)
C            if (icode.ne.0) goto 900
C            call rangei(nout,'IXXXXX',ival,ixxmin,ixxmax)
C            ixxxxx(isub1) = ival
C         else
Cc           here, there was no subscript.
Cc           read values until next variable or $end
C            isub1 = 1
C XYZ        continue
C            call getiv(nin,line,length,iptr,lineno,ival,icode)
C            if (icode.eq.-1) goto 20
C            if (icode.ne.0) goto 900
C            call rangei(nout,'IXXXXX',ival,ixxmin,ixxmax)
C            ixxxxx(isub1) = ival
C            isub1 = isub1 + 1
C            goto XYZ
C         end if
C
C  Notes
C  =====
C  This code does not handle array bound checking.
C  This code does not handle multiple subscripts without minor changes,
C   i.e. it can handle
C        IXXX(1) = 43
C   or   IXXX = 43, 27, 31, 33 etc.
C   and  XXXX = 3., 1.2E-1, .5 etc
C   but not
C        XXXX(1,1) = 2.0 (but see RDNL24 for a method that works)
C  Where 2D arrays are concerned, e.g.
C        XXXX = 3., 1.2E-1, .5 etc
C  it is assumed that the FIRST subscript varies fastest, i.e. this
C  statement is equivalent to
C        XXXX(1,1) = 3.,
C        XXXX(2,1) = 1.2E-1,
C        XXXX(3,1) = .5
C        etc.
C
C  Note : The code will fail if the $name is preceded on the line
C  by a space.
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.6368
C
C--Date
C  22 May 2007
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C  21/03/97 PJK 1.100 Added new routine RDNL24
C  22/05/07 PJK 1.200 Added new routine RDNL25
C
C--Arguments
C  ifunit : (INPUT)  Fortran input unit identifier
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'labels.h'

C  Arguments
      INTEGER icode,ifunit

C  Local variables
      CHARACTER*200 error,line
      CHARACTER*32 nlname
      INTEGER errlen,iost,iptr,length,lineno,nllen

C  External functions
      INTEGER  lenstr
      EXTERNAL lenstr

C  External routines
      EXTERNAL oheadr,uppcas,nlerr,rdnl01,rdnl02,rdnl03,rdnl04,rdnl05,
     +     rdnl06,rdnl07,rdnl08,rdnl09,rdnl10,rdnl11,rdnl12,rdnl13,
     +     rdnl14,rdnl15,rdnl16,rdnl17,rdnl18,rdnl19,rdnl20,rdnl21,
     +     rdnl22,rdnl23,rdnl24,rdnl25

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      call oheadr(nout,'Input Parameters')

      lineno = 0

C *** Find first line starting $name

 10   continue
      read(ifunit,'(a)',iostat=iost) line

C *** On error or end, return

      if (iost.ne.0) goto 1000

      lineno = lineno + 1
      length = lenstr(line,len(line))

C *** Ignore blank lines

      if (line.eq.' ') goto 10

C *** Ignore comments, unless they start with '*****',
C *** in which case print them.

      if (line(1:5).eq.'*****') then
         write(nout,*) line(1:76)
      end if

      if (line(1:1).eq.'*') goto 10

C *** We are looking for a namelist name

      if (line(1:1).ne.'$') then
         error= 'Unable to find namelist block name - use $name...$end'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,' ',0)
         icode = 1
         goto 1000
      end if

C *** Extract the namelist name

      iptr = index(line,' ') - 1
      if (iptr.le.0) iptr = length
      nlname = line(2:iptr)
      nllen = (iptr - 2) + 1

C *** Convert namelist name to upper case

      call uppcas(nlname,1,nllen)

C *** See which namelist block it is

      if (nlname(1:nllen).eq.'INPT1') then
         call rdnl01(lineno,icode)

      else if (nlname(1:nllen).eq.'PHYDAT') then
         call rdnl02(lineno,icode)

      else if (nlname(1:nllen).eq.'INEQDAT') then
         call rdnl03(lineno,icode)

      else if (nlname(1:nllen).eq.'CDDAT') then
         call rdnl04(lineno,icode)

      else if (nlname(1:nllen).eq.'TIME') then
         call rdnl05(lineno,icode)

      else if (nlname(1:nllen).eq.'DIVT') then
         call rdnl06(lineno,icode)

      else if (nlname(1:nllen).eq.'BLD') then
         call rdnl07(lineno,icode)

      else if (nlname(1:nllen).eq.'TFC') then
         call rdnl08(lineno,icode)

      else if (nlname(1:nllen).eq.'PFC') then
         call rdnl09(lineno,icode)

      else if (nlname(1:nllen).eq.'PULSE') then
         call rdnl10(lineno,icode)

      else if (nlname(1:nllen).eq.'FWBLSH') then
         call rdnl11(lineno,icode)

      else if (nlname(1:nllen).eq.'HTPWR') then
         call rdnl12(lineno,icode)

      else if (nlname(1:nllen).eq.'COSTINP') then
         call rdnl13(lineno,icode)

      else if (nlname(1:nllen).eq.'UCSTINP') then
         call rdnl14(lineno,icode)

      else if (nlname(1:nllen).eq.'SWEP') then
         call rdnl15(lineno,icode)

      else if (nlname(1:nllen).eq.'BLDINP') then
         call rdnl16(lineno,icode)

      else if (nlname(1:nllen).eq.'HTTINP') then
         call rdnl17(lineno,icode)

      else if (nlname(1:nllen).eq.'HTRINP') then
         call rdnl18(lineno,icode)

      else if (nlname(1:nllen).eq.'EST') then
         call rdnl19(lineno,icode)

      else if (nlname(1:nllen).eq.'BCOM') then
         call rdnl20(lineno,icode)

      else if (nlname(1:nllen).eq.'OSECTS') then
         call rdnl21(lineno,icode)

      else if (nlname(1:nllen).eq.'VACCY') then
         call rdnl22(lineno,icode)

      else if (nlname(1:nllen).eq.'STELLA') then
         call rdnl23(lineno,icode)

      else if (nlname(1:nllen).eq.'IFE') then
         call rdnl24(lineno,icode)

      else if (nlname(1:nllen).eq.'HPLANT') then
         call rdnl25(lineno,icode)

C *** Invalid namelist

      else
         error=' The following name is an unknown namelist block name:'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,nlname,nllen)
         icode = 1
      end if

      if (icode.eq.0) goto 10

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL01(lineno,icode)

C  Handle the NAMELIST input data for block 'INPT1'.
C
C  Reads data in NAMELIST format
C
C  $INPT1
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/inpt1/
c  +     boundl, boundu, epsfcn, epsvmc, factor, ftol, icc, ioptimz,
c  +     ivms, ixc, maxcal, minmax, neqns, nvar
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'labels.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data
      if (varnam(1:varlen).eq.'BOUNDL') then
         if (isub1.ne.9999) then
c           Here, there was a subscript
            oldval = boundl(isub1)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            boundl(isub1) = rval
            if (boundl(isub1).ne.oldval) then
               write(nout,*) 'The LOWER bound of variable ',
     +              lablxc(isub1),'             = ',boundl(isub1)
            end if
         else
c           Here, there was no subscript.
c           Read values until next variable or $end
            isub1 = 1
 100        continue
            oldval = boundl(isub1)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            boundl(isub1) = rval
            if (boundl(isub1).ne.oldval) then
               write(nout,*) 'The LOWER bound of variable ',
     +              lablxc(isub1),'             = ',boundl(isub1)
            end if
            isub1 = isub1 + 1
            goto 100
         end if
      else if (varnam(1:varlen).eq.'BOUNDU') then
         if (isub1.ne.9999) then
c           Here, there was a subscript
            oldval = boundu(isub1)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            boundu(isub1) = rval
            if (boundu(isub1).ne.oldval) then
               write(nout,*) 'The UPPER bound of variable ',
     +              lablxc(isub1),'             = ',boundu(isub1)
            end if
         else
c           Here, there was no subscript.
c           Read values until next variable or $end
            isub1 = 1
 110        continue
            oldval = boundu(isub1)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            boundu(isub1) = rval
            if (boundu(isub1).ne.oldval) then
               write(nout,*) 'The UPPER bound of variable ',
     +              lablxc(isub1),'             = ',boundu(isub1)
            end if
            isub1 = isub1 + 1
            goto 110
         end if
      else if (varnam(1:varlen).eq.'EPSFCN') then
         if (isub1.ne.9999) goto 910
         oldval = epsfcn
         call getrv(nin,line,length,iptr,lineno,epsfcn,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'EPSFCN',epsfcn,0.0D0,1.0D0)
         if (epsfcn.ne.oldval) then
            clabel = 'HYBRID derivative step length,'
            write(nout,*) clabel,varnam(1:8),' = ',epsfcn
         end if
      else if (varnam(1:varlen).eq.'EPSVMC') then
         if (isub1.ne.9999) goto 910
         oldval = epsvmc
         call getrv(nin,line,length,iptr,lineno,epsvmc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'EPSVMC',epsvmc,0.0D0,1.0D0)
         if (epsvmc.ne.oldval) then
            clabel = 'VMCON error tolerance,'
            write(nout,*) clabel,varnam(1:8),' = ',epsvmc
         end if
      else if (varnam(1:varlen).eq.'FACTOR') then
         if (isub1.ne.9999) goto 910
         oldval = factor
         call getrv(nin,line,length,iptr,lineno,factor,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FACTOR',factor,0.0D0,10.0D0)
         if (factor.ne.oldval) then
            clabel = 'VMCON initial step size,'
            write(nout,*) clabel,varnam(1:8),' = ',factor
         end if
      else if (varnam(1:varlen).eq.'FTOL') then
         if (isub1.ne.9999) goto 910
         oldval = ftol
         call getrv(nin,line,length,iptr,lineno,ftol,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FTOL',ftol,0.0D0,1.0D0)
         if (ftol.ne.oldval) then
            clabel = 'HYBRID tolerance,'
            write(nout,*) clabel,varnam(1:8),' = ',ftol
         end if
      else if (varnam(1:varlen).eq.'ICC') then
         if (isub1.ne.9999) then
c           Here, there was a subscript
            call getiv(nin,line,length,iptr,lineno,ival,icode)
            if (icode.ne.0) goto 900
            icc(isub1) = ival
         else
c           Here, there was no subscript.
c           Read values until next variable or $end
            isub1 = 1
 120        continue
            call getiv(nin,line,length,iptr,lineno,ival,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            icc(isub1) = ival
            isub1 = isub1 + 1
            goto 120
         end if
      else if (varnam(1:varlen).eq.'IOPTIMZ') then
         if (isub1.ne.9999) goto 910
         ioldvl = ioptimz
         call getiv(nin,line,length,iptr,lineno,ioptimz,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IOPTIMZ',ioptimz,-1,1)
         if (ioptimz.ne.ioldvl) then
            clabel = 'Switch for HYBRID/VMCON,'
            write(nout,*) clabel,varnam(1:8),' = ',ioptimz
            if (ioptimz.lt.0) then
               write(nout,*) '     (HYBRID run, no optimisation)'
            else if (ioptimz.eq.0) then
               write(nout,*) '     (Both HYBRID and VMCON runs)'
            else
               write(nout,*) '     (VMCON optimisation run)'
            end if
         end if
C+**PJK 17/11/97
      else if (varnam(1:varlen).eq.'IVMS') then
         if (isub1.ne.9999) goto 910
         ioldvl = ivms
         call getiv(nin,line,length,iptr,lineno,ivms,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IVMS',ivms,0,1)
         if (ivms.ne.ioldvl) then
            clabel = 'Switch for Operating System,'
            write(nout,*) clabel,varnam(1:8),' = ',ivms
            if (ivms.eq.0) then
               write(nout,*)
     +              '     (IBM RS/6000 AIX XL Fortran 3.2 compiler)'
            else
               write(nout,*) '     (ALPHA OpenVMS compiler)'
            end if
         end if
C-**PJK 17/11/97
      else if (varnam(1:varlen).eq.'IXC') then
         if (isub1.ne.9999) then
c           Here, there was a subscript
            call getiv(nin,line,length,iptr,lineno,ival,icode)
            if (icode.ne.0) goto 900
            ixc(isub1) = ival
         else
c           Here, there was no subscript.
c           Read values until next variable or $end
            isub1 = 1
 130        continue
            call getiv(nin,line,length,iptr,lineno,ival,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            ixc(isub1) = ival
            isub1 = isub1 + 1
            goto 130
         end if
      else if (varnam(1:varlen).eq.'MAXCAL') then
         if (isub1.ne.9999) goto 910
         ioldvl = maxcal
         call getiv(nin,line,length,iptr,lineno,maxcal,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'MAXCAL',maxcal,0,10000)
         if (maxcal.ne.ioldvl) then
            clabel = 'Max no of VMCON iterations,'
            write(nout,*) clabel,varnam(1:8),' = ',maxcal
         end if
      else if (varnam(1:varlen).eq.'MINMAX') then
         if (isub1.ne.9999) goto 910
         ioldvl = minmax
         call getiv(nin,line,length,iptr,lineno,minmax,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'MINMAX',minmax,-ipnfoms,ipnfoms)
         if (minmax.ne.ioldvl) then
            clabel = 'Switch for figure of merit,'
            write(nout,*) clabel,varnam(1:8),' = ',minmax
            if (minmax.lt.0) then
               clbl = '     (F-O-M calculation will maximise '
            else
               clbl = '     (F-O-M calculation will minimise '
            end if
            if (abs(minmax).eq.1) then
               clbl2 = 'major radius)'
            else if (abs(minmax).eq.2) then
               clbl2 = 'p_fus / p_in)'
            else if (abs(minmax).eq.3) then
               clbl2 = 'neutron wall load)'
            else if (abs(minmax).eq.4) then
               clbl2 = 'p_tf + p_pf)'
            else if (abs(minmax).eq.5) then
               clbl2 = 'p_fus / p_inj)'
            else if (abs(minmax).eq.6) then
               clbl2 = 'cost of electricity)'
            else if (abs(minmax).eq.7) then
               clbl2 = 'capital cost)'
            else if (abs(minmax).eq.8) then
               clbl2 = 'aspect ratio)'
            else if (abs(minmax).eq.9) then
               clbl2 = 'divertor heat load)'
            else if (abs(minmax).eq.10) then
               clbl2 = 'toroidal field on axis)'
            else if (abs(minmax).eq.11) then
               clbl2 = 'injection power)'
            else if (abs(minmax).eq.12) then
               clbl2 = 'H plant capital cost)'
            else if (abs(minmax).eq.13) then
               clbl2 = 'H production rate)'
            else
               clbl2 = 'UNKNOWN...)'
            end if
            write(nout,*) clbl,clbl2
         end if
      else if (varnam(1:varlen).eq.'NEQNS') then
         if (isub1.ne.9999) goto 910
         ioldvl = neqns
         call getiv(nin,line,length,iptr,lineno,neqns,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'NEQNS',neqns,1,ipeqns)
         if (neqns.ne.ioldvl) then
            clabel = 'No of constraint equations,'
            write(nout,*) clabel,varnam(1:8),' = ',neqns
         end if

C+**PJK 26/01/93 Removed NINEQNS, as it need not be changed from zero

      else if (varnam(1:varlen).eq.'NVAR') then
         if (isub1.ne.9999) goto 910
         ioldvl = nvar
         call getiv(nin,line,length,iptr,lineno,nvar,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'NVAR',nvar,1,ipnvars)
         if (nvar.ne.ioldvl) then
            clabel = 'No of independent variables,'
            write(nout,*) clabel,varnam(1:8),' = ',nvar
         end if
      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block INPT1'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000
c
 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000
c
 920  continue
      error = ' Unknown variable in namelist block INPT1'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000
c
 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL02(lineno,icode)

C  Handle the NAMELIST input data for block 'PHYDAT'.
C
C  Reads data in NAMELIST format
C
C  $PHYDAT
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/phydat/
c  +     abeam, alphaj, alphan, alphat, aspect, beamfus0, beta, betbm0,
c  +     bt, carea, cfe0, csawth, cvol, dene, dign, dnbeta, epbetmax,
c  +     falpha, fbfe, fdeut, ffwal, fhe3, fradmin, ftr, ftrit,
c  +     fvsbrnni, gamma, gtscale, hfact, ibss, iculbl, iculcr, iculdl,
c  +     icurr, idensl, idhe3, idivrt, ifalphap, ifispact, igeom,
c  +     ignite, iinvqd, iiter, impc, impfe, impo, ires, isc, iscrp,
c  +     ishape, itart, iwalld, kappa, q, q0, ralpne, recyle, rfpth,
c  +     rli, rmajor, rnbeam, rtpte, ssync, te, ti, tratio, triang
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'labels.h'
      INCLUDE 'phydat.h'
      INCLUDE 'rfp.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ABEAM') then
         if (isub1.ne.9999) goto 910
         oldval = abeam
         call getrv(nin,line,length,iptr,lineno,abeam,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ABEAM',abeam,0.0D0,3.0D0)
         if (abeam.ne.oldval) then
            clabel = 'Beam ion mass,'
            write(nout,*) clabel,varnam(1:8),' = ',abeam
         end if
      else if (varnam(1:varlen).eq.'ALPHAJ') then
         if (isub1.ne.9999) goto 910
         oldval = alphaj
         call getrv(nin,line,length,iptr,lineno,alphaj,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ALPHAJ',alphaj,0.0D0,10.0D0)
         if (alphaj.ne.oldval) then
            clabel = 'Current density profile factor,'
            write(nout,*) clabel,varnam(1:8),' = ',alphaj
         end if
      else if (varnam(1:varlen).eq.'ALPHAN') then
         if (isub1.ne.9999) goto 910
         oldval = alphan
         call getrv(nin,line,length,iptr,lineno,alphan,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ALPHAN',alphan,0.0D0,10.0D0)
         if (alphan.ne.oldval) then
            clabel = 'Density profile factor,'
            write(nout,*) clabel,varnam(1:8),' = ',alphan
         end if
      else if (varnam(1:varlen).eq.'ALPHAT') then
         if (isub1.ne.9999) goto 910
         oldval = alphat
         call getrv(nin,line,length,iptr,lineno,alphat,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ALPHAT',alphat,0.0D0,10.0D0)
         if (alphat.ne.oldval) then
            clabel = 'Temperature profile factor,'
            write(nout,*) clabel,varnam(1:8),' = ',alphat
         end if
      else if (varnam(1:varlen).eq.'ASPECT') then
         if (isub1.ne.9999) goto 910
         oldval = aspect
         call getrv(nin,line,length,iptr,lineno,aspect,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ASPECT',aspect,1.001D0,20.0D0)
         if (aspect.ne.oldval) then
            clabel = 'Aspect ratio,'
            write(nout,*) clabel,varnam(1:8),' = ',aspect
         end if
      else if (varnam(1:varlen).eq.'BEAMFUS0') then
         if (isub1.ne.9999) goto 910
         oldval = beamfus0
         call getrv(nin,line,length,iptr,lineno,beamfus0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BEAMFUS0',beamfus0,0.01D0,10.0D0)
         if (beamfus0.ne.oldval) then
            clabel = 'Beam-bkg fusion multiplier,'
            write(nout,*) clabel,varnam(1:8),' = ',beamfus0
         end if
      else if (varnam(1:varlen).eq.'BETA') then
         if (isub1.ne.9999) goto 910
         oldval = beta
         call getrv(nin,line,length,iptr,lineno,beta,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BETA',beta,0.0D0,1.0D0)
         if (beta.ne.oldval) then
            clabel = 'Plasma beta,'
            write(nout,*) clabel,varnam(1:8),' = ',beta
         end if
      else if (varnam(1:varlen).eq.'BETBM0') then
         if (isub1.ne.9999) goto 910
         oldval = betbm0
         call getrv(nin,line,length,iptr,lineno,betbm0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BETBM0',betbm0,0.0D0,10.0D0)
         if (betbm0.ne.oldval) then
            clabel = 'leading coeff. for nb beta fraction,'
            write(nout,*) clabel,varnam(1:8),' = ',betbm0
         end if
      else if (varnam(1:varlen).eq.'BT') then
         if (isub1.ne.9999) goto 910
         oldval = bt
         call getrv(nin,line,length,iptr,lineno,bt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BT',bt,0.0D0,20.0D0)
         if (bt.ne.oldval) then
            clabel = 'Toroidal field on axis (t),'
            write(nout,*) clabel,varnam(1:8),' = ',bt
         end if
      else if (varnam(1:varlen).eq.'CAREA') then
         if (isub1.ne.9999) goto 910
         oldval = carea
         call getrv(nin,line,length,iptr,lineno,carea,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CAREA',carea,0.01D0,10.0D0)
         if (carea.ne.oldval) then
            clabel = 'Plasma surface area multiplier,'
            write(nout,*) clabel,varnam(1:8),' = ',carea
         end if
      else if (varnam(1:varlen).eq.'CFE0') then
         if (isub1.ne.9999) goto 910
         oldval = cfe0
         call getrv(nin,line,length,iptr,lineno,cfe0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CFE0',cfe0,0.0D0,10.0D0)
         if (cfe0.ne.oldval) then
            clabel = 'Additional Fe impurity fraction,'
            write(nout,*) clabel,varnam(1:8),' = ',cfe0
         end if
      else if (varnam(1:varlen).eq.'CSAWTH') then
         if (isub1.ne.9999) goto 910
         oldval = csawth
         call getrv(nin,line,length,iptr,lineno,csawth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CSAWTH',csawth,0.0D0,10.0D0)
         if (csawth.ne.oldval) then
            clabel = 'Coefficient for sawteeth effects,'
            write(nout,*) clabel,varnam(1:8),' = ',csawth
         end if
      else if (varnam(1:varlen).eq.'CVOL') then
         if (isub1.ne.9999) goto 910
         oldval = cvol
         call getrv(nin,line,length,iptr,lineno,cvol,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CVOL',cvol,0.01D0,10.0D0)
         if (cvol.ne.oldval) then
            clabel = 'Plasma volume multiplier,'
            write(nout,*) clabel,varnam(1:8),' = ',cvol
         end if
      else if (varnam(1:varlen).eq.'DENE') then
         if (isub1.ne.9999) goto 910
         oldval = dene
         call getrv(nin,line,length,iptr,lineno,dene,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DENE',dene,1.0D18,1.0D22)
         if (dene.ne.oldval) then
            clabel = 'Electron density (/m3),'
            write(nout,*) clabel,varnam(1:8),' = ',dene
         end if
      else if (varnam(1:varlen).eq.'DIGN') then
         if (isub1.ne.9999) goto 910
         oldval = dign
         call getrv(nin,line,length,iptr,lineno,dign,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DIGN',dign,0.01D0,100.0D0)
         if (dign.ne.oldval) then
            clabel = 'Guess for ignition margin,'
            write(nout,*) clabel,varnam(1:8),' = ',dign
         end if
      else if (varnam(1:varlen).eq.'DNBETA') then
         if (isub1.ne.9999) goto 910
         oldval = dnbeta
         call getrv(nin,line,length,iptr,lineno,dnbeta,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DNBETA',dnbeta,0.3D0,20.0D0)
         if (dnbeta.ne.oldval) then
            clabel = 'Troyon beta coefficient,'
            write(nout,*) clabel,varnam(1:8),' = ',dnbeta
         end if
      else if (varnam(1:varlen).eq.'EPBETMAX') then
         if (isub1.ne.9999) goto 910
         oldval = epbetmax
         call getrv(nin,line,length,iptr,lineno,epbetmax,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'EPBETMAX',epbetmax,0.01D0,10.0D0)
         if (epbetmax.ne.oldval) then
            clabel = 'Max epsilon*beta value,'
            write(nout,*) clabel,varnam(1:8),' = ',epbetmax
         end if
      else if (varnam(1:varlen).eq.'FALPHA') then
         if (isub1.ne.9999) goto 910
         oldval = falpha
         call getrv(nin,line,length,iptr,lineno,falpha,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FALPHA',falpha,0.0D0,1.0D0)
         if (falpha.ne.oldval) then
            clabel = 'Fraction of alpha power deposited to plasma,'
            write(nout,*) clabel,varnam(1:8),' = ',falpha
         end if
      else if (varnam(1:varlen).eq.'FBFE') then
         if (isub1.ne.9999) goto 910
         oldval = fbfe
         call getrv(nin,line,length,iptr,lineno,fbfe,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBFE',fbfe,0.0D0,1.0D0)
         if (fbfe.ne.oldval) then
            clabel = 'Fraction of Fe radn to Bremsstrahlung,'
            write(nout,*) clabel,varnam(1:8),' = ',fbfe
         end if
      else if (varnam(1:varlen).eq.'FDEUT') then
         if (isub1.ne.9999) goto 910
         oldval = fdeut
         call getrv(nin,line,length,iptr,lineno,fdeut,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FDEUT',fdeut,0.0D0,1.0D0)
         if (fdeut.ne.oldval) then
            clabel = 'Deuterium fuel fraction in D-He3 reaction,'
            write(nout,*) clabel,varnam(1:8),' = ',fdeut
         end if
      else if (varnam(1:varlen).eq.'FFWAL') then
         if (isub1.ne.9999) goto 910
         oldval = ffwal
         call getrv(nin,line,length,iptr,lineno,ffwal,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FFWAL',ffwal,0.0D0,10.0D0)
         if (ffwal.ne.oldval) then
            clabel = 'Wall load fiddle factor,'
            write(nout,*) clabel,varnam(1:8),' = ',ffwal
         end if
      else if (varnam(1:varlen).eq.'FHE3') then
         if (isub1.ne.9999) goto 910
         oldval = fhe3
         call getrv(nin,line,length,iptr,lineno,fhe3,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FHE3',fhe3,0.0D0,1.0D0)
         if (fhe3.ne.oldval) then
            clabel = 'Helium-3 fuel fraction in D-He3 reaction,'
            write(nout,*) clabel,varnam(1:8),' = ',fhe3
         end if
      else if (varnam(1:varlen).eq.'FRADMIN') then
         if (isub1.ne.9999) goto 910
         oldval = fradmin
         call getrv(nin,line,length,iptr,lineno,fradmin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FRADMIN',fradmin,0.0D0,1.0D0)
         if (fradmin.ne.oldval) then
            clabel = 'F-value for minimum radiation power,'
            write(nout,*) clabel,varnam(1:8),' = ',fradmin
         end if
      else if (varnam(1:varlen).eq.'FTR') then
         if (isub1.ne.9999) goto 910
         oldval = ftr
         call getrv(nin,line,length,iptr,lineno,ftr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FTR',ftr,0.0D0,1.0D0)
         if (ftr.ne.oldval) then
            clabel = 'Tritium fraction of DT ions,'
            write(nout,*) clabel,varnam(1:8),' = ',ftr
         end if
      else if (varnam(1:varlen).eq.'FTRIT') then
         if (isub1.ne.9999) goto 910
         oldval = ftrit
         call getrv(nin,line,length,iptr,lineno,ftrit,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FTRIT',ftrit,0.0D0,1.0D0)
         if (ftrit.ne.oldval) then
            clabel = 'Tritium fuel fraction in D-He3 reaction,'
            write(nout,*) clabel,varnam(1:8),' = ',ftrit
         end if
      else if (varnam(1:varlen).eq.'FVSBRNNI') then
         if (isub1.ne.9999) goto 910
         oldval = fvsbrnni
         call getrv(nin,line,length,iptr,lineno,fvsbrnni,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FVSBRNNI',fvsbrnni,0.0D0,1.0D0)
         if (fvsbrnni.ne.oldval) then
            clabel = 'Non-inductive volt-sec burn fraction,'
            write(nout,*) clabel,varnam(1:8),' = ',fvsbrnni
         end if
      else if (varnam(1:varlen).eq.'GAMMA') then
         if (isub1.ne.9999) goto 910
         oldval = gamma
         call getrv(nin,line,length,iptr,lineno,gamma,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'GAMMA',gamma,0.1D0,1.0D0)
         if (gamma.ne.oldval) then
            clabel = 'Coefficient for resistive V-s formula,'
            write(nout,*) clabel,varnam(1:8),' = ',gamma
         end if
      else if (varnam(1:varlen).eq.'GTSCALE') then
         if (isub1.ne.9999) goto 910
         ioldvl = gtscale
         call getiv(nin,line,length,iptr,lineno,gtscale,icode)
         if (icode.ne.0) goto 900
         if (gtscale.ne.ioldvl) then
            clabel = 'Flag to scale Troyon coefficient,'
            write(nout,*) clabel,varnam(1:8),' = ',gtscale
         end if
      else if (varnam(1:varlen).eq.'HFACT') then
         if (isub1.ne.9999) goto 910
         oldval = hfact
         call getrv(nin,line,length,iptr,lineno,hfact,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'HFACT',hfact,0.01D0,10.0D0)
         if (hfact.ne.oldval) then
            clabel = 'Energy confinement time H factor,'
            write(nout,*) clabel,varnam(1:8),' = ',hfact
         end if

C+**PJK 09/11/92 Removed redundant variable IBETIN

      else if (varnam(1:varlen).eq.'IBSS') then
         if (isub1.ne.9999) goto 910
         ioldvl = ibss
         call getiv(nin,line,length,iptr,lineno,ibss,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IBSS',ibss,1,3)
         if (ibss.ne.ioldvl) then
            clabel = 'Switch for bootstrap scaling,'
            write(nout,*) clabel,varnam(1:8),' = ',ibss
            if (ibss.eq.1) then
               write(nout,*)
     +              '     (ITER bootstrap scaling - high R/a only)'
            else if (ibss.eq.2) then
               write(nout,*)
     +          '     (General bootstrap scaling - valid for all R/a)'
            else if (ibss.eq.3) then
               write(nout,*)
     +              '     (Culham scaling - valid for all R/a)'
            else
               continue
            end if
         end if
      else if (varnam(1:varlen).eq.'ICULBL') then
         if (isub1.ne.9999) goto 910
         ioldvl = iculbl
         call getiv(nin,line,length,iptr,lineno,iculbl,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ICULBL',iculbl,0,2)
         if (iculbl.ne.ioldvl) then
            clabel = 'Switch for Troyon beta limit scaling,'
            write(nout,*) clabel,varnam(1:8),' = ',iculbl
            if (iculbl.eq.0) then
               write(nout,*)
     +              '     (Original method)'
            else if (iculbl.eq.1) then
               write(nout,*)
     +              '     (Beta limit applies to thermal beta only)'
            else if (iculbl.eq.2) then
               write(nout,*)
     +              '     (Beta limit applies to thermal+NB beta only)'
            else
               continue
            end if
         end if
      else if (varnam(1:varlen).eq.'ICULCR') then
         if (isub1.ne.9999) goto 910
         ioldvl = iculcr
         call getiv(nin,line,length,iptr,lineno,iculcr,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ICULCR',iculcr,0,1)
         if (iculcr.ne.ioldvl) then
            clabel = 'Switch for plasma current scaling,'
            write(nout,*) clabel,varnam(1:8),' = ',iculcr
            if (iculcr.eq.0) then
               write(nout,*)
     +              '     (Four to choose from)'
            else
               write(nout,*)
     +              '     (Seven to choose from)'
            end if
         end if
      else if (varnam(1:varlen).eq.'ICULDL') then
         if (isub1.ne.9999) goto 910
         ioldvl = iculdl
         call getiv(nin,line,length,iptr,lineno,iculdl,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ICULDL',iculdl,0,1)
         if (iculdl.ne.ioldvl) then
            clabel = 'Switch for density limit formulae,'
            write(nout,*) clabel,varnam(1:8),' = ',iculdl
            if (iculdl.eq.0) then
               write(nout,*)
     +              '     (Original formula)'
            else
               write(nout,*)
     +              '     (Six to choose from)'
            end if
         end if
      else if (varnam(1:varlen).eq.'ICURR') then
         if (isub1.ne.9999) goto 910
         ioldvl = icurr
         call getiv(nin,line,length,iptr,lineno,icurr,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ICURR',icurr,1,7)
         if ((iculcr.eq.0).and.(icurr.gt.4)) then
            write(nout,*) 'Error in input file:'
            write(nout,*) 'Illegal value of ICURR, = ',icurr
            write(nout,*) 'Change value of ICULCR to 1'
            write(nout,*) 'BEFORE setting ICURR.'
            write(nout,*) 'PROCESS stopping.'
            STOP
         end if
         if (icurr.ne.ioldvl) then
            clabel = 'Switch for plasma current scaling,'
            write(nout,*) clabel,varnam(1:8),' = ',icurr
            if (icurr.eq.1) then
               write(nout,*) '     (Peng analytical fit)'
            else if (icurr.eq.2) then
               write(nout,*) '     (Peng divertor scaling (TART))'
            else if (icurr.eq.3) then
               write(nout,*) '     (Simple ITER scaling)'
            else if (icurr.eq.4) then
               write(nout,*) '     (Later ITER scaling - Uckan)'
            else if (icurr.eq.5) then
               write(nout,*) '     (Todd scaling I)'
            else if (icurr.eq.6) then
               write(nout,*) '     (Todd scaling II)'
            else
               write(nout,*) '     (Connor-Hastie scaling)'
            end if
         end if
C+**PJK 17/09/97 Added Greenwald limit (idensl=7)
      else if (varnam(1:varlen).eq.'IDENSL') then
         if (isub1.ne.9999) goto 910
         ioldvl = idensl
         call getiv(nin,line,length,iptr,lineno,idensl,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IDENSL',idensl,1,7)
         if (idensl.ne.ioldvl) then
            clabel = 'Switch for enforced density limit,'
            write(nout,*) clabel,varnam(1:8),' = ',idensl
            if (idensl.eq.1) then
               write(nout,*) '     (Old ASDEX model)'
            else if (idensl.eq.2) then
               write(nout,*) '     (Borrass model for ITER, I)'
            else if (idensl.eq.3) then
               write(nout,*) '     (Borrass model for ITER, II)'
            else if (idensl.eq.4) then
               write(nout,*) '     (JET edge radiation model)'
            else if (idensl.eq.5) then
               write(nout,*) '     (Simplified JET model)'
            else if (idensl.eq.6) then
               write(nout,*) '     (Hugill-Murakami Mq limit)'
            else
               write(nout,*) '     (Greenwald limit)'
            end if
         end if
C+**PJK 04/12/95 Added IDHE3
      else if (varnam(1:varlen).eq.'IDHE3') then
         if (isub1.ne.9999) goto 910
         ioldvl = idhe3
         call getiv(nin,line,length,iptr,lineno,idhe3,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IDHE3',idhe3,0,1)
         if (idhe3.ne.ioldvl) then
            clabel = 'Switch for main fusion reaction,'
            write(nout,*) clabel,varnam(1:8),' = ',idhe3
            if (idhe3.eq.0) then
               write(nout,*) '     (Deuterium-Tritium)'
            else
               write(nout,*) '     (Deuterium-Helium3)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IDIVRT') then
         if (isub1.ne.9999) goto 910
         ioldvl = idivrt
         call getiv(nin,line,length,iptr,lineno,idivrt,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IDIVRT',idivrt,2,2)
         if (idivrt.ne.ioldvl) then
            clabel = 'Switch for divertor configuration,'
            write(nout,*) clabel,varnam(1:8),' = ',idivrt
            if (idivrt.eq.0) then
               write(nout,*) '     (Limiter - DO NOT USE)'
            else if (idivrt.eq.1) then
               write(nout,*) '     (Single Null - DO NOT USE)'
            else
               write(nout,*) '     (Double Null)'
            end if
         end if
C+**PJK 22/05/06 Added IFALPHAP
      else if (varnam(1:varlen).eq.'IFALPHAP') then
         if (isub1.ne.9999) goto 910
         ioldvl = ifalphap
         call getiv(nin,line,length,iptr,lineno,ifalphap,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IFALPHAP',ifalphap,0,1)
         if (ifalphap.ne.ioldvl) then
            clabel = 'Switch for fast alpha pressure fit,'
            write(nout,*) clabel,varnam(1:8),' = ',ifalphap
            if (ifalphap.eq.0) then
               write(nout,*) '     (ITER Uckan)'
            else
               write(nout,*) '     (Modified DJW)'
            end if
         end if
C+**PJK 27/02/96 Added IFISPACT
      else if (varnam(1:varlen).eq.'IFISPACT') then
         if (isub1.ne.9999) goto 910
         ioldvl = ifispact
         call getiv(nin,line,length,iptr,lineno,ifispact,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IFISPACT',ifispact,0,1)
         if (ifispact.ne.ioldvl) then
            clabel = 'Switch for neutronics calculations,'
            write(nout,*) clabel,varnam(1:8),' = ',ifispact
            if (ifispact.eq.0) then
               write(nout,*) '     (turned off)'
            else
               write(nout,*) '     (turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IGEOM') then
         if (isub1.ne.9999) goto 910
         ioldvl = igeom
         call getiv(nin,line,length,iptr,lineno,igeom,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IGEOM',igeom,0,1)
         if (igeom.ne.ioldvl) then
            clabel = 'Switch for plasma geometry calculation,'
            write(nout,*) clabel,varnam(1:8),' = ',igeom
            if (igeom.eq.0) then
               write(nout,*) '     (Original method)'
            else
               write(nout,*) '     (New method)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IGNITE') then
         if (isub1.ne.9999) goto 910
         ioldvl = ignite
         call getiv(nin,line,length,iptr,lineno,ignite,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IGNITE',ignite,0,1)
         if (ignite.ne.ioldvl) then
            clabel = 'Switch for ignited plasma assumption,'
            write(nout,*) clabel,varnam(1:8),' = ',ignite
            if (ignite.eq.0) then
               write(nout,*) '     (Plasma not ignited)'
            else
               write(nout,*) '     (Plasma ignited)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IINVQD') then
         if (isub1.ne.9999) goto 910
         ioldvl = iinvqd
         call getiv(nin,line,length,iptr,lineno,iinvqd,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IINVQD',iinvqd,0,1)
         if (iinvqd.ne.ioldvl) then
            clabel = 'Switch for inverse quadrature,'
            write(nout,*) clabel,varnam(1:8),' = ',iinvqd
            if (iinvqd.eq.0) then
               write(nout,*) '     (Inverse quadrature turned off)'
            else
               write(nout,*) '     (Inverse quadrature turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IITER') then
         if (isub1.ne.9999) goto 910
         ioldvl = iiter
         call getiv(nin,line,length,iptr,lineno,iiter,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IITER',iiter,0,1)
         if (iiter.ne.ioldvl) then
            clabel = 'Switch for ITER fusion power calcs,'
            write(nout,*) clabel,varnam(1:8),' = ',iiter
            if (iiter.eq.0) then
               write(nout,*) '     (Calculations turned off)'
            else
               write(nout,*) '     (Calculations turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IMPC') then
         if (isub1.ne.9999) goto 910
         oldval = impc
         call getrv(nin,line,length,iptr,lineno,impc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'IMPC',impc,0.0D0,10.0D0)
         if (impc.ne.oldval) then
            clabel = 'Carbon impurity multiplier,'
            write(nout,*) clabel,varnam(1:8),' = ',impc
         end if
      else if (varnam(1:varlen).eq.'IMPFE') then
         if (isub1.ne.9999) goto 910
         oldval = impfe
         call getrv(nin,line,length,iptr,lineno,impfe,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'IMPFE',impfe,0.0D0,2.0D0)
         if (impfe.ne.oldval) then
            clabel = 'Iron impurity multiplier,'
            write(nout,*) clabel,varnam(1:8),' = ',impfe
         end if
      else if (varnam(1:varlen).eq.'IMPO') then
         if (isub1.ne.9999) goto 910
         oldval = impo
         call getrv(nin,line,length,iptr,lineno,impo,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'IMPO',impo,0.0D0,10.0D0)
         if (impo.ne.oldval) then
            clabel = 'Oxygen impurity multiplier,'
            write(nout,*) clabel,varnam(1:8),' = ',impo
         end if
      else if (varnam(1:varlen).eq.'IRES') then
         if (isub1.ne.9999) goto 910
         ioldvl = ires
         call getiv(nin,line,length,iptr,lineno,ires,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IRES',ires,0,1)
         if (ires.ne.ioldvl) then
            clabel = 'Switch for neo-cl. plasma resistivity,'
            write(nout,*) clabel,varnam(1:8),' = ',ires
            if (ires.eq.0) then
               write(nout,*) '     (Neo-classical value is not used)'
            else
               write(nout,*) '     (Neo-classical value is used)'
            end if
         end if
      else if (varnam(1:varlen).eq.'ISC') then
         if (isub1.ne.9999) goto 910
         ioldvl = isc
         call getiv(nin,line,length,iptr,lineno,isc,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ISC',isc,1,ipnlaws)
         if (isc.ne.ioldvl) then
            clabel = 'Switch for confinement scaling law,'
            write(nout,*) clabel,varnam(1:8),' = ',isc
            write(nout,*) '     ( ',tauscl(isc),' )'
         end if
      else if (varnam(1:varlen).eq.'ISCRP') then
         if (isub1.ne.9999) goto 910
         ioldvl = iscrp
         call getiv(nin,line,length,iptr,lineno,iscrp,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ISCRP',iscrp,0,1)
         if (iscrp.ne.ioldvl) then
            clabel = 'Switch for scrapeoff width,'
            write(nout,*) clabel,varnam(1:8),' = ',iscrp
            if (iscrp.eq.0) then
               write(nout,*) '     (10% of minor radius is used)'
            else
               write(nout,*) '     (Input values are used)'
            end if
         end if
      else if (varnam(1:varlen).eq.'ISHAPE') then
         if (isub1.ne.9999) goto 910
         ioldvl = ishape
         call getiv(nin,line,length,iptr,lineno,ishape,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ISHAPE',ishape,0,1)
         if (ishape.ne.ioldvl) then
            clabel = 'Switch for plasma shape vs. aspect,'
            write(nout,*) clabel,varnam(1:8),' = ',ishape
            if (ishape.eq.0) then
               write(nout,*) '     (Use input values for kappa,triang)'
            else
               write(nout,*) '     (Scale kappa, triang, qlim)'
            end if
         end if

C+**PJK 08/12/93 Removed obsolete variable ISTOK

      else if (varnam(1:varlen).eq.'ITART') then
         if (isub1.ne.9999) goto 910
         ioldvl = itart
         call getiv(nin,line,length,iptr,lineno,itart,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ITART',itart,0,1)
         if (itart.ne.ioldvl) then
            clabel = 'Switch for tight aspect ratio physics,'
            write(nout,*) clabel,varnam(1:8),' = ',itart
            if (itart.eq.0) then
               write(nout,*) '     (Conventional aspect ratio)'
            else
               write(nout,*) '     (Tight aspect ratio model)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IWALLD') then
         if (isub1.ne.9999) goto 910
         ioldvl = iwalld
         call getiv(nin,line,length,iptr,lineno,iwalld,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IWALLD',iwalld,1,2)
         if (iwalld.ne.ioldvl) then
            clabel = 'Switch for wall load calculation,'
            write(nout,*) clabel,varnam(1:8),' = ',iwalld
            if (iwalld.eq.1) then
               write(nout,*) '     (Scale from plasma surface area)'
            else
               write(nout,*) '     (Use first wall area directly)'
            end if
         end if

c  Note: kappa is declared as double precision

      else if (varnam(1:varlen).eq.'KAPPA') then
         if (isub1.ne.9999) goto 910
         oldval = kappa
         call getrv(nin,line,length,iptr,lineno,kappa,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'KAPPA',kappa,0.99D0,5.0D0)
         if (kappa.ne.oldval) then
            clabel = 'Plasma separatrix elongation,'
            write(nout,*) clabel,varnam(1:8),' = ',kappa
         end if

C+**PJK 09/11/92 Removed redundant variable POHC

      else if (varnam(1:varlen).eq.'Q') then
         if (isub1.ne.9999) goto 910
         oldval = q
         call getrv(nin,line,length,iptr,lineno,q,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'Q',q,0.01D0,50.0D0)
         if (q.ne.oldval) then
            clabel = 'Edge safety factor,'
            write(nout,*) clabel,varnam(1:8),' = ',q
         end if
      else if (varnam(1:varlen).eq.'Q0') then
         if (isub1.ne.9999) goto 910
         oldval = q0
         call getrv(nin,line,length,iptr,lineno,q0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'Q0',q0,0.01D0,20.0D0)
         if (q0.ne.oldval) then
            clabel = 'Safety factor on axis,'
            write(nout,*) clabel,varnam(1:8),' = ',q0
         end if
      else if (varnam(1:varlen).eq.'RALPNE') then
         if (isub1.ne.9999) goto 910
         oldval = ralpne
         call getrv(nin,line,length,iptr,lineno,ralpne,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RALPNE',ralpne,0.001D0,1.0D0)
         if (ralpne.ne.oldval) then
            clabel = 'Thermal alpha density / electron density,'
            write(nout,*) clabel,varnam(1:8),' = ',ralpne
         end if
      else if (varnam(1:varlen).eq.'RECYLE') then
         if (isub1.ne.9999) goto 910
         oldval = recyle
         call getrv(nin,line,length,iptr,lineno,recyle,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RECYLE',recyle,0.0D0,1.0D0)
         if (recyle.ne.oldval) then
            clabel = 'Alpha recycle to main plasma,'
            write(nout,*) clabel,varnam(1:8),' = ',recyle
         end if
C+**PJK 06/03/96
      else if (varnam(1:varlen).eq.'RFPTH') then
         if (isub1.ne.9999) goto 910
         oldval = rfpth
         call getrv(nin,line,length,iptr,lineno,rfpth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RFPTH',rfpth,0.01D0,1.8D0)
         if (rfpth.ne.oldval) then
            clabel = 'RFP pinch parameter, theta,'
            write(nout,*) clabel,varnam(1:8),' = ',rfpth
         end if
      else if (varnam(1:varlen).eq.'RLI') then
         if (isub1.ne.9999) goto 910
         oldval = rli
         call getrv(nin,line,length,iptr,lineno,rli,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RLI',rli,0.0D0,10.0D0)
         if (rli.ne.oldval) then
            clabel = 'Normalised inductivity,'
            write(nout,*) clabel,varnam(1:8),' = ',rli
         end if
      else if (varnam(1:varlen).eq.'RMAJOR') then
         if (isub1.ne.9999) goto 910
         oldval = rmajor
         call getrv(nin,line,length,iptr,lineno,rmajor,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RMAJOR',rmajor,0.1D0,30.0D0)
         if (rmajor.ne.oldval) then
            clabel = 'Plasma major radius (m),'
            write(nout,*) clabel,varnam(1:8),' = ',rmajor
         end if
      else if (varnam(1:varlen).eq.'RNBEAM') then
         if (isub1.ne.9999) goto 910
         oldval = rnbeam
         call getrv(nin,line,length,iptr,lineno,rnbeam,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RNBEAM',rnbeam,0.0D0,1.0D0)
         if (rnbeam.ne.oldval) then
            clabel = 'Hot beam density / electron density,'
            write(nout,*) clabel,varnam(1:8),' = ',rnbeam
         end if
      else if (varnam(1:varlen).eq.'RTPTE') then
         if (isub1.ne.9999) goto 910
         oldval = rtpte
         call getrv(nin,line,length,iptr,lineno,rtpte,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RTPTE',rtpte,0.1D0,20.0D0)
         if (rtpte.ne.oldval) then
            clabel = 'He particle / plas. energy confin. time,'
            write(nout,*) clabel,varnam(1:8),' = ',rtpte
         end if

C+**PJK 17/12/92 Moved SCRAPLI and SCRAPLO to block BLD in RDNL07
C+**PJK 09/11/92 Removed redundant variable SCRAPT

      else if (varnam(1:varlen).eq.'SSYNC') then
         if (isub1.ne.9999) goto 910
         oldval = ssync
         call getrv(nin,line,length,iptr,lineno,ssync,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SSYNC',ssync,0.0D0,1.0D0)
         if (ssync.ne.oldval) then
            clabel = 'Synchrotron wall reflectivity factor,'
            write(nout,*) clabel,varnam(1:8),' = ',ssync
         end if
      else if (varnam(1:varlen).eq.'TE') then
         if (isub1.ne.9999) goto 910
         oldval = te
         call getrv(nin,line,length,iptr,lineno,te,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TE',te,2.0D0,150.0D0)
         if (te.ne.oldval) then
            clabel = 'Electron temperature (keV),'
            write(nout,*) clabel,varnam(1:8),' = ',te
         end if
      else if (varnam(1:varlen).eq.'TI') then
         if (isub1.ne.9999) goto 910
         oldval = ti
         call getrv(nin,line,length,iptr,lineno,ti,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TI',ti,5.0D0,50.0D0)
         if (ti.ne.oldval) then
            clabel = 'Ion temperature (keV),'
            write(nout,*) clabel,varnam(1:8),' = ',ti
         end if
      else if (varnam(1:varlen).eq.'TRATIO') then
         if (isub1.ne.9999) goto 910
         oldval = tratio
         call getrv(nin,line,length,iptr,lineno,tratio,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TRATIO',tratio,0.0D0,2.0D0)
         if (tratio.ne.oldval) then
            clabel = 'Ion / electron temperature ratio,'
            write(nout,*) clabel,varnam(1:8),' = ',tratio
         end if
      else if (varnam(1:varlen).eq.'TRIANG') then
         if (isub1.ne.9999) goto 910
         oldval = triang
         call getrv(nin,line,length,iptr,lineno,triang,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TRIANG',triang,0.0D0,1.0D0)
         if (triang.ne.oldval) then
            clabel = 'Plasma separatrix triangularity,'
            write(nout,*) clabel,varnam(1:8),' = ',triang
         end if

C+**PJK 09/11/92 Removed redundant variable TRIA

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block PHYDAT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block PHYDAT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL03(lineno,icode)

C  Handle the NAMELIST input data for block 'INEQDAT'.
C
C  Reads data in NAMELIST format
C
C  $INEQDAT
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/ineqdat/
c  +     auxmin, betpmx, bmxlim, dtmpmx, fauxmn, fbeta, fbetap,
c  +     fbetatry, fdene, fdivcol, fdtmp, fgamcd, fipir, fjohc,
c  +     fjohc0, fjtfc, fhldiv, ffuspow, fiooic, fjprot, fmva, fpeakb,
c  +     fpinj, fpnetel, fportsz, fptemp, fq, fqval, frfpf, frfptf,
c  +     frminor, fstrcase, fstrcond, ftburn, ftcycl, ftmargtf, ftohs,
c  +     ftpeak, fvdump, fwalld, fvs, gammax, mvalim, pnetelin,
c  +     powfmax, tbrnmx, tpkmax, walalw
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'ineq.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

C+**PJK 10/11/92 Removed redundant variable BMAX0
C+**PJK 10/11/92 Removed redundant variable FBEAM

      if (varnam(1:varlen).eq.'AUXMIN') then
         if (isub1.ne.9999) goto 910
         oldval = auxmin
         call getrv(nin,line,length,iptr,lineno,auxmin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'AUXMIN',auxmin,0.01D0,100.0D0)
         if (auxmin.ne.oldval) then
            clabel = 'Minimum auxiliary power (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',auxmin
         end if
C+**PJK 06/03/96
      else if (varnam(1:varlen).eq.'BETPMX') then
         if (isub1.ne.9999) goto 910
         oldval = betpmx
         call getrv(nin,line,length,iptr,lineno,betpmx,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BETPMX',betpmx,0.01D0,2.0D0)
         if (betpmx.ne.oldval) then
            clabel = 'Maximum poloidal beta,'
            write(nout,*) clabel,varnam(1:8),' = ',betpmx
         end if
      else if (varnam(1:varlen).eq.'BMXLIM') then
         if (isub1.ne.9999) goto 910
         oldval = bmxlim
         call getrv(nin,line,length,iptr,lineno,bmxlim,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BMXLIM',bmxlim,0.1D0,50.0D0)
         if (bmxlim.ne.oldval) then
            clabel = 'Maximum toroidal field (T),'
            write(nout,*) clabel,varnam(1:8),' = ',bmxlim
         end if
      else if (varnam(1:varlen).eq.'DTMPMX') then
         if (isub1.ne.9999) goto 910
         oldval = dtmpmx
         call getrv(nin,line,length,iptr,lineno,dtmpmx,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DTMPMX',dtmpmx,1.0D0,1000.0D0)
         if (dtmpmx.ne.oldval) then
            clabel = 'Maximum temp rise in f.w. coolant (K),'
            write(nout,*) clabel,varnam(1:8),' = ',dtmpmx
         end if
      else if (varnam(1:varlen).eq.'FAUXMN') then
         if (isub1.ne.9999) goto 910
         oldval = fauxmn
         call getrv(nin,line,length,iptr,lineno,fauxmn,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FAUXMN',fauxmn,0.001D0,10.0D0)
         if (fauxmn.ne.oldval) then
            clabel = 'F-value for minimum auxiliary power,'
            write(nout,*) clabel,varnam(1:8),' = ',fauxmn
         end if
      else if (varnam(1:varlen).eq.'FBETA') then
         if (isub1.ne.9999) goto 910
         oldval = fbeta
         call getrv(nin,line,length,iptr,lineno,fbeta,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBETA',fbeta,0.001D0,10.0D0)
         if (fbeta.ne.oldval) then
            clabel = 'F-value for eps.betap beta limit,'
            write(nout,*) clabel,varnam(1:8),' = ',fbeta
         end if
C+**PJK 06/03/96
      else if (varnam(1:varlen).eq.'FBETAP') then
         if (isub1.ne.9999) goto 910
         oldval = fbetap
         call getrv(nin,line,length,iptr,lineno,fbetap,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBETAP',fbetap,0.001D0,10.0D0)
         if (fbetap.ne.oldval) then
            clabel = 'F-value for poloidal beta limit,'
            write(nout,*) clabel,varnam(1:8),' = ',fbetap
         end if
      else if (varnam(1:varlen).eq.'FBETATRY') then
         if (isub1.ne.9999) goto 910
         oldval = fbetatry
         call getrv(nin,line,length,iptr,lineno,fbetatry,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBETATRY',fbetatry,0.001D0,10.0D0)
         if (fbetatry.ne.oldval) then
            clabel = 'F-value for Troyon beta limit,'
            write(nout,*) clabel,varnam(1:8),' = ',fbetatry
         end if

C+**PJK 10/11/92 Removed redundant variable FBMAX

      else if (varnam(1:varlen).eq.'FDENE') then
         if (isub1.ne.9999) goto 910
         oldval = fdene
         call getrv(nin,line,length,iptr,lineno,fdene,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FDENE',fdene,0.001D0,10.0D0)
         if (fdene.ne.oldval) then
            clabel = 'F-value for density limit,'
            write(nout,*) clabel,varnam(1:8),' = ',fdene
         end if
      else if (varnam(1:varlen).eq.'FDIVCOL') then
         if (isub1.ne.9999) goto 910
         oldval = fdivcol
         call getrv(nin,line,length,iptr,lineno,fdivcol,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FDIVCOL',fdivcol,0.001D0,10.0D0)
         if (fdivcol.ne.oldval) then
            clabel = 'F-value for divertor collisionality,'
            write(nout,*) clabel,varnam(1:8),' = ',fdivcol
         end if
      else if (varnam(1:varlen).eq.'FDTMP') then
         if (isub1.ne.9999) goto 910
         oldval = fdtmp
         call getrv(nin,line,length,iptr,lineno,fdtmp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FDTMP',fdtmp,0.001D0,10.0D0)
         if (fdtmp.ne.oldval) then
            clabel = 'F-value for first wall coolant temp rise,'
            write(nout,*) clabel,varnam(1:8),' = ',fdtmp
         end if
      else if (varnam(1:varlen).eq.'FGAMCD') then
         if (isub1.ne.9999) goto 910
         oldval = fgamcd
         call getrv(nin,line,length,iptr,lineno,fgamcd,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FGAMCD',fgamcd,0.001D0,10.0D0)
         if (fgamcd.ne.oldval) then
            clabel = 'F-value for current drive gamma,'
            write(nout,*) clabel,varnam(1:8),' = ',fgamcd
         end if
C+**PJK 31/01/96
      else if (varnam(1:varlen).eq.'FIPIR') then
         if (isub1.ne.9999) goto 910
         oldval = fipir
         call getrv(nin,line,length,iptr,lineno,fipir,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FIPIR',fipir,0.001D0,10.0D0)
         if (fipir.ne.oldval) then
            clabel = 'F-value for Ip/Irod,'
            write(nout,*) clabel,varnam(1:8),' = ',fipir
         end if
      else if (varnam(1:varlen).eq.'FJOHC') then
         if (isub1.ne.9999) goto 910
         oldval = fjohc
         call getrv(nin,line,length,iptr,lineno,fjohc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FJOHC',fjohc,0.001D0,10.0D0)
         if (fjohc.ne.oldval) then
            clabel = 'F-value for OH coil current at EOF,'
            write(nout,*) clabel,varnam(1:8),' = ',fjohc
         end if
      else if (varnam(1:varlen).eq.'FJOHC0') then
         if (isub1.ne.9999) goto 910
         oldval = fjohc0
         call getrv(nin,line,length,iptr,lineno,fjohc0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FJOHC0',fjohc0,0.001D0,10.0D0)
         if (fjohc0.ne.oldval) then
            clabel = 'F-value for OH coil current at BOP,'
            write(nout,*) clabel,varnam(1:8),' = ',fjohc0
         end if
      else if (varnam(1:varlen).eq.'FJTFC') then
         if (isub1.ne.9999) goto 910
         oldval = fjtfc
         call getrv(nin,line,length,iptr,lineno,fjtfc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FJTFC',fjtfc,0.001D0,10.0D0)
         if (fjtfc.ne.oldval) then
            clabel = 'F-value for tf coil current density,'
            write(nout,*) clabel,varnam(1:8),' = ',fjtfc
         end if
      else if (varnam(1:varlen).eq.'FHLDIV') then
         if (isub1.ne.9999) goto 910
         oldval = fhldiv
         call getrv(nin,line,length,iptr,lineno,fhldiv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FHLDIV',fhldiv,0.001D0,10.0D0)
         if (fhldiv.ne.oldval) then
            clabel = 'F-value for divertor heat load,'
            write(nout,*) clabel,varnam(1:8),' = ',fhldiv
         end if

C+**PJK 10/11/92 Removed redundant variable FFIGMR
C+**PJK 10/11/92 Removed redundant variable FGMRIN

      else if (varnam(1:varlen).eq.'FFUSPOW') then
         if (isub1.ne.9999) goto 910
         oldval = ffuspow
         call getrv(nin,line,length,iptr,lineno,ffuspow,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FFUSPOW',ffuspow,0.001D0,10.0D0)
         if (ffuspow.ne.oldval) then
            clabel = 'F-value for maximum fusion power,'
            write(nout,*) clabel,varnam(1:8),' = ',ffuspow
         end if
      else if (varnam(1:varlen).eq.'FIOOIC') then
         if (isub1.ne.9999) goto 910
         oldval = fiooic
         call getrv(nin,line,length,iptr,lineno,fiooic,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FIOOIC',fiooic,0.001D0,10.0D0)
         if (fiooic.ne.oldval) then
            clabel = 'F-value for SCTF iop/icrit,'
            write(nout,*) clabel,varnam(1:8),' = ',fiooic
         end if
      else if (varnam(1:varlen).eq.'FJPROT') then
         if (isub1.ne.9999) goto 910
         oldval = fjprot
         call getrv(nin,line,length,iptr,lineno,fjprot,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FJPROT',fjprot,0.001D0,10.0D0)
         if (fjprot.ne.oldval) then
            clabel = 'F-value for SCTF winding pack J,'
            write(nout,*) clabel,varnam(1:8),' = ',fjprot
         end if
      else if (varnam(1:varlen).eq.'FMVA') then
         if (isub1.ne.9999) goto 910
         oldval = fmva
         call getrv(nin,line,length,iptr,lineno,fmva,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMVA',fmva,0.001D0,10.0D0)
         if (fmva.ne.oldval) then
            clabel = 'F-value for maximum MVA,'
            write(nout,*) clabel,varnam(1:8),' = ',fmva
         end if
      else if (varnam(1:varlen).eq.'FPEAKB') then
         if (isub1.ne.9999) goto 910
         oldval = fpeakb
         call getrv(nin,line,length,iptr,lineno,fpeakb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FPEAKB',fpeakb,0.001D0,10.0D0)
         if (fpeakb.ne.oldval) then
            clabel = 'F-value for max toroidal field,'
            write(nout,*) clabel,varnam(1:8),' = ',fpeakb
         end if
      else if (varnam(1:varlen).eq.'FPINJ') then
         if (isub1.ne.9999) goto 910
         oldval = fpinj
         call getrv(nin,line,length,iptr,lineno,fpinj,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FPINJ',fpinj,0.001D0,10.0D0)
         if (fpinj.ne.oldval) then
            clabel = 'F-value for injection power,'
            write(nout,*) clabel,varnam(1:8),' = ',fpinj
         end if
      else if (varnam(1:varlen).eq.'FPNETEL') then
         if (isub1.ne.9999) goto 910
         oldval = fpnetel
         call getrv(nin,line,length,iptr,lineno,fpnetel,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FPNETEL',fpnetel,0.001D0,10.0D0)
         if (fpnetel.ne.oldval) then
            clabel = 'F-value for net electric power,'
            write(nout,*) clabel,varnam(1:8),' = ',fpnetel
         end if
      else if (varnam(1:varlen).eq.'FPORTSZ') then
         if (isub1.ne.9999) goto 910
         oldval = fportsz
         call getrv(nin,line,length,iptr,lineno,fportsz,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FPORTSZ',fportsz,0.001D0,10.0D0)
         if (fportsz.ne.oldval) then
            clabel = 'F-value for port size,'
            write(nout,*) clabel,varnam(1:8),' = ',fportsz
         end if
      else if (varnam(1:varlen).eq.'FPTEMP') then
         if (isub1.ne.9999) goto 910
         oldval = fptemp
         call getrv(nin,line,length,iptr,lineno,fptemp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FPTEMP',fptemp,0.001D0,10.0D0)
         if (fptemp.ne.oldval) then
            clabel = 'F-value for peak centrepost temperature,'
            write(nout,*) clabel,varnam(1:8),' = ',fptemp
         end if
      else if (varnam(1:varlen).eq.'FQ') then
         if (isub1.ne.9999) goto 910
         oldval = fq
         call getrv(nin,line,length,iptr,lineno,fq,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FQ',fq,0.001D0,10.0D0)
         if (fq.ne.oldval) then
            clabel = 'F-value for edge safety factor,'
            write(nout,*) clabel,varnam(1:8),' = ',fq
         end if
      else if (varnam(1:varlen).eq.'FQVAL') then
         if (isub1.ne.9999) goto 910
         oldval = fqval
         call getrv(nin,line,length,iptr,lineno,fqval,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FQVAL',fqval,0.001D0,10.0D0)
         if (fqval.ne.oldval) then
            clabel = 'F-value for energy multiplication Q,'
            write(nout,*) clabel,varnam(1:8),' = ',fqval
         end if
C+**PJK 06/03/96
      else if (varnam(1:varlen).eq.'FRFPF') then
         if (isub1.ne.9999) goto 910
         oldval = frfpf
         call getrv(nin,line,length,iptr,lineno,frfpf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FRFPF',frfpf,0.001D0,10.0D0)
         if (frfpf.ne.oldval) then
            clabel = 'F-value for RFP reversal parameter,'
            write(nout,*) clabel,varnam(1:8),' = ',frfpf
         end if
C+**PJK 28/02/96
      else if (varnam(1:varlen).eq.'FRFPTF') then
         if (isub1.ne.9999) goto 910
         oldval = frfptf
         call getrv(nin,line,length,iptr,lineno,frfptf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FRFPTF',frfptf,0.001D0,1.0D0)
         if (frfptf.ne.oldval) then
            clabel = 'F-value for TF coil toroidal thickness,'
            write(nout,*) clabel,varnam(1:8),' = ',frfptf
         end if
      else if (varnam(1:varlen).eq.'FRMINOR') then
         if (isub1.ne.9999) goto 910
         oldval = frminor
         call getrv(nin,line,length,iptr,lineno,frminor,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FRMINOR',frminor,0.001D0,10.0D0)
         if (frminor.ne.oldval) then
            clabel = 'F-value for minor radius limit,'
            write(nout,*) clabel,varnam(1:8),' = ',frminor
         end if
      else if (varnam(1:varlen).eq.'FSTRCASE') then
         if (isub1.ne.9999) goto 910
         oldval = fstrcase
         call getrv(nin,line,length,iptr,lineno,fstrcase,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FSTRCASE',fstrcase,0.001D0,10.0D0)
         if (fstrcase.ne.oldval) then
            clabel = 'F-value for TF coil case stress,'
            write(nout,*) clabel,varnam(1:8),' = ',fstrcase
         end if
      else if (varnam(1:varlen).eq.'FSTRCOND') then
         if (isub1.ne.9999) goto 910
         oldval = fstrcond
         call getrv(nin,line,length,iptr,lineno,fstrcond,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FSTRCOND',fstrcond,0.001D0,10.0D0)
         if (fstrcond.ne.oldval) then
            clabel = 'F-value for TF coil conduit stress,'
            write(nout,*) clabel,varnam(1:8),' = ',fstrcond
         end if
      else if (varnam(1:varlen).eq.'FTBURN') then
         if (isub1.ne.9999) goto 910
         oldval = ftburn
         call getrv(nin,line,length,iptr,lineno,ftburn,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FTBURN',ftburn,0.001D0,10.0D0)
         if (ftburn.ne.oldval) then
            clabel = 'F-value for burn time limit,'
            write(nout,*) clabel,varnam(1:8),' = ',ftburn
         end if
      else if (varnam(1:varlen).eq.'FTCYCL') then
         if (isub1.ne.9999) goto 910
         oldval = ftcycl
         call getrv(nin,line,length,iptr,lineno,ftcycl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FTCYCL',ftcycl,0.001D0,10.0D0)
         if (ftcycl.ne.oldval) then
            clabel = 'F-value for cycle time,'
            write(nout,*) clabel,varnam(1:8),' = ',ftcycl
         end if
      else if (varnam(1:varlen).eq.'FTMARGTF') then
         if (isub1.ne.9999) goto 910
         oldval = ftmargtf
         call getrv(nin,line,length,iptr,lineno,ftmargtf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FTMARGTF',ftmargtf,0.001D0,10.0D0)
         if (ftmargtf.ne.oldval) then
            clabel = 'F-value for TF coil temp. margin,'
            write(nout,*) clabel,varnam(1:8),' = ',ftmargtf
         end if
      else if (varnam(1:varlen).eq.'FTOHS') then
         if (isub1.ne.9999) goto 910
         oldval = ftohs
         call getrv(nin,line,length,iptr,lineno,ftohs,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FTOHS',ftohs,0.001D0,10.0D0)
         if (ftohs.ne.oldval) then
            clabel = 'F-value for OH coil swing time,'
            write(nout,*) clabel,varnam(1:8),' = ',ftohs
         end if
      else if (varnam(1:varlen).eq.'FTPEAK') then
         if (isub1.ne.9999) goto 910
         oldval = ftpeak
         call getrv(nin,line,length,iptr,lineno,ftpeak,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FTPEAK',ftpeak,0.001D0,10.0D0)
         if (ftpeak.ne.oldval) then
            clabel = 'F-value for peak first wall temperature,'
            write(nout,*) clabel,varnam(1:8),' = ',ftpeak
         end if
      else if (varnam(1:varlen).eq.'FVDUMP') then
         if (isub1.ne.9999) goto 910
         oldval = fvdump
         call getrv(nin,line,length,iptr,lineno,fvdump,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FVDUMP',fvdump,0.001D0,10.0D0)
         if (fvdump.ne.oldval) then
            clabel = 'F-value for dump voltage,'
            write(nout,*) clabel,varnam(1:8),' = ',fvdump
         end if
      else if (varnam(1:varlen).eq.'FWALLD') then
         if (isub1.ne.9999) goto 910
         oldval = fwalld
         call getrv(nin,line,length,iptr,lineno,fwalld,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FWALLD',fwalld,0.001D0,10.0D0)
         if (fwalld.ne.oldval) then
            clabel = 'F-value for wall load limit,'
            write(nout,*) clabel,varnam(1:8),' = ',fwalld
         end if
      else if (varnam(1:varlen).eq.'FVS') then
         if (isub1.ne.9999) goto 910
         oldval = fvs
         call getrv(nin,line,length,iptr,lineno,fvs,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FVS',fvs,0.001D0,10.0D0)
         if (fvs.ne.oldval) then
            clabel = 'F-value for startup V-s requirement,'
            write(nout,*) clabel,varnam(1:8),' = ',fvs
         end if
      else if (varnam(1:varlen).eq.'GAMMAX') then
         if (isub1.ne.9999) goto 910
         oldval = gammax
         call getrv(nin,line,length,iptr,lineno,gammax,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'GAMMAX',gammax,0.01D0,10.0D0)
         if (gammax.ne.oldval) then
            clabel = 'Maximum current drive gamma (A/W-m2),'
            write(nout,*) clabel,varnam(1:8),' = ',gammax
         end if
      else if (varnam(1:varlen).eq.'MVALIM') then
         if (isub1.ne.9999) goto 910
         oldval = mvalim
         call getrv(nin,line,length,iptr,lineno,mvalim,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'MVALIM',mvalim,0.0D0,1000.0D0)
         if (mvalim.ne.oldval) then
            clabel = 'Maximum MVA limit,'
            write(nout,*) clabel,varnam(1:8),' = ',mvalim
         end if
      else if (varnam(1:varlen).eq.'PNETELIN') then
         if (isub1.ne.9999) goto 910
         oldval = pnetelin
         call getrv(nin,line,length,iptr,lineno,pnetelin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PNETELIN',pnetelin,1.0D0,1.0D4)
         if (pnetelin.ne.oldval) then
            clabel = 'Required net electric power (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',pnetelin
         end if
      else if (varnam(1:varlen).eq.'POWFMAX') then
         if (isub1.ne.9999) goto 910
         oldval = powfmax
         call getrv(nin,line,length,iptr,lineno,powfmax,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'POWFMAX',powfmax,1.0D0,1.0D4)
         if (powfmax.ne.oldval) then
            clabel = 'Maximum fusion power (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',powfmax
         end if
      else if (varnam(1:varlen).eq.'TBRNMN') then
         if (isub1.ne.9999) goto 910
         oldval = tbrnmn
         call getrv(nin,line,length,iptr,lineno,tbrnmn,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TBRNMN',tbrnmn,1.0D-3,1.0D6)
         if (tbrnmn.ne.oldval) then
            clabel = 'Minimum burn time (s),'
            write(nout,*) clabel,varnam(1:8),' = ',tbrnmn
         end if
      else if (varnam(1:varlen).eq.'TPKMAX') then
         if (isub1.ne.9999) goto 910
         oldval = tpkmax
         call getrv(nin,line,length,iptr,lineno,tpkmax,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TPKMAX',tpkmax,100.0D0,1.0D3)
         if (tpkmax.ne.oldval) then
            clabel = 'Maximum first wall peak temperature (C),'
            write(nout,*) clabel,varnam(1:8),' = ',tpkmax
         end if
      else if (varnam(1:varlen).eq.'WALALW') then
         if (isub1.ne.9999) goto 910
         oldval = walalw
         call getrv(nin,line,length,iptr,lineno,walalw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'WALALW',walalw,0.001D0,50.0D0)
         if (walalw.ne.oldval) then
            clabel = 'Allowable wall load (MW/m2),'
            write(nout,*) clabel,varnam(1:8),' = ',walalw
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block INEQDAT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block INEQDAT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL04(lineno,icode)

C  Handle the NAMELIST input data for block 'CDDAT'.
C
C  Reads data in NAMELIST format
C
C  $CDDAT
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/cddat/
c  +     beamwd, bscfmax, cboot, echpwr0, enbeam, etalh, etaech,
c  +     etanbi, etaof, feffcd, frbeam, ftritbm, iefrf, irfcd, pheat,
c  +     pinjalw, tbeamin
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'cdriv.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'BEAMWD') then
         if (isub1.ne.9999) goto 910
         oldval = beamwd
         call getrv(nin,line,length,iptr,lineno,beamwd,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BEAMWD',beamwd,0.001D0,5.0D0)
         if (beamwd.ne.oldval) then
            clabel = 'Beam width (m),'
            write(nout,*) clabel,varnam(1:8),' = ',beamwd
         end if
      else if (varnam(1:varlen).eq.'BSCFMAX') then
         if (isub1.ne.9999) goto 910
         oldval = bscfmax
         call getrv(nin,line,length,iptr,lineno,bscfmax,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BSCFMAX',bscfmax,-0.999D0,0.999D0)
         if (bscfmax.ne.oldval) then
            clabel = 'Bootstrap fraction,'
            write(nout,*) clabel,varnam(1:8),' = ',bscfmax
            if (bscfmax.lt.0.0d0) then
               write(nout,*)
     + '     (Bootstrap current fraction is FIXED at ',abs(bscfmax),' )'
            else
               write(nout,*)
     + '     (MAXIMUM bootstrap current fraction = ',bscfmax,' )'
            end if
         end if
      else if (varnam(1:varlen).eq.'CBOOT') then
         if (isub1.ne.9999) goto 910
         oldval = cboot
         call getrv(nin,line,length,iptr,lineno,cboot,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CBOOT',cboot,0.0D0,10.0D0)
         if (cboot.ne.oldval) then
            clabel = 'Bootstrap current fraction multiplier,'
            write(nout,*) clabel,varnam(1:8),' = ',cboot
         end if
      else if (varnam(1:varlen).eq.'ECHPWR0') then
         if (isub1.ne.9999) goto 910
         oldval = echpwr0
         call getrv(nin,line,length,iptr,lineno,echpwr0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ECHPWR0',echpwr0,0.0D0,100.0D6)
         if (echpwr0.ne.oldval) then
            clabel = 'Startup ECH power (W),'
            write(nout,*) clabel,varnam(1:8),' = ',echpwr0
         end if
      else if (varnam(1:varlen).eq.'ENBEAM') then
         if (isub1.ne.9999) goto 910
         oldval = enbeam
         call getrv(nin,line,length,iptr,lineno,enbeam,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ENBEAM',enbeam,1.0D0,20.0D3)
         if (enbeam.ne.oldval) then
            clabel = 'Neutral beam energy (keV),'
            write(nout,*) clabel,varnam(1:8),' = ',enbeam
         end if
      else if (varnam(1:varlen).eq.'ETALH') then
         if (isub1.ne.9999) goto 910
         oldval = etalh
         call getrv(nin,line,length,iptr,lineno,etalh,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETALH',etalh,0.0D0,1.0D0)
         if (etalh.ne.oldval) then
            clabel = 'LH wall plug to plasma efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etalh
         end if
      else if (varnam(1:varlen).eq.'ETAECH') then
         if (isub1.ne.9999) goto 910
         oldval = etaech
         call getrv(nin,line,length,iptr,lineno,etaech,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAECH',etaech,0.0D0,1.0D0)
         if (etaech.ne.oldval) then
            clabel = 'ECH wall plug to injector efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etaech
         end if
      else if (varnam(1:varlen).eq.'ETANBI') then
         if (isub1.ne.9999) goto 910
         oldval = etanbi
         call getrv(nin,line,length,iptr,lineno,etanbi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETANBI',etanbi,0.0D0,1.0D0)
         if (etanbi.ne.oldval) then
            clabel = 'NBI wall plug to injector efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etanbi
         end if
C+**PJK 06/03/96
      else if (varnam(1:varlen).eq.'ETAOF') then
         if (isub1.ne.9999) goto 910
         oldval = etaof
         call getrv(nin,line,length,iptr,lineno,etaof,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAOF',etaof,0.0D0,1.0D0)
         if (etaof.ne.oldval) then
            clabel = 'OFCD wall plug to injector efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etaof
         end if
      else if (varnam(1:varlen).eq.'FEFFCD') then
         if (isub1.ne.9999) goto 910
         oldval = feffcd
         call getrv(nin,line,length,iptr,lineno,feffcd,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FEFFCD',feffcd,0.0D0,20.0D0)
         if (feffcd.ne.oldval) then
            clabel = 'Current drive efficiency fiddle factor,'
            write(nout,*) clabel,varnam(1:8),' = ',feffcd
         end if
      else if (varnam(1:varlen).eq.'FRBEAM') then
         if (isub1.ne.9999) goto 910
         oldval = frbeam
         call getrv(nin,line,length,iptr,lineno,frbeam,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FRBEAM',frbeam,0.5D0,2.0D0)
         if (frbeam.ne.oldval) then
            clabel = 'R_tan / R_major for NBI,'
            write(nout,*) clabel,varnam(1:8),' = ',frbeam
         end if
      else if (varnam(1:varlen).eq.'FTRITBM') then
         if (isub1.ne.9999) goto 910
         oldval = ftritbm
         call getrv(nin,line,length,iptr,lineno,ftritbm,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FTRITBM',ftritbm,0.0D0,1.0D0)
         if (ftritbm.ne.oldval) then
            clabel = 'Tritium fraction of beam,'
            write(nout,*) clabel,varnam(1:8),' = ',ftritbm
         end if
      else if (varnam(1:varlen).eq.'IEFRF') then
         if (isub1.ne.9999) goto 910
         ioldvl = iefrf
         call getiv(nin,line,length,iptr,lineno,iefrf,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IEFRF',iefrf,1,9)
         if (iefrf.ne.ioldvl) then
            clabel = 'Switch for curr drive efficiency model,'
            write(nout,*) clabel,varnam(1:8),' = ',iefrf
            if (iefrf.eq.1) then
               write(nout,*) '     (Fenstermacher Lower Hybrid)'
            else if (iefrf.eq.2) then
               write(nout,*) '     (INTOR Lower Hybrid)'
            else if (iefrf.eq.3) then
               write(nout,*) '     (Fenstermacher ECRH)'
            else if (iefrf.eq.4) then
               write(nout,*) '     (Ehst Lower Hybrid)'
            else if (iefrf.eq.5) then
               write(nout,*) '     (ITER Neutral Beam)'
            else if (iefrf.eq.6) then
               write(nout,*) '     (ITER ECRH)'
            else if (iefrf.eq.7) then
               write(nout,*) '     (Culham Lower Hybrid model)'
            else if (iefrf.eq.8) then
               write(nout,*) '     (Culham Neutral Beam model)'
            else
               write(nout,*) '     (RFP OFCD model)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IRFCD') then
         if (isub1.ne.9999) goto 910
         ioldvl = irfcd
         call getiv(nin,line,length,iptr,lineno,irfcd,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IRFCD',irfcd,0,1)
         if (irfcd.ne.ioldvl) then
            clabel = 'Switch for current drive calculation,'
            write(nout,*) clabel,varnam(1:8),' = ',irfcd
            if (irfcd.eq.0) then
               write(nout,*) '     (No current drive will be used)'
            else
               write(nout,*) '     (Current drive will be used)'
            end if
         end if
      else if (varnam(1:varlen).eq.'PHEAT') then
         if (isub1.ne.9999) goto 910
         oldval = pheat
         call getrv(nin,line,length,iptr,lineno,pheat,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PHEAT',pheat,0.0D0,1.0D9)
         if (pheat.ne.oldval) then
            clabel = 'Heating power not used for C.D. (W),'
            write(nout,*) clabel,varnam(1:8),' = ',pheat
         end if
      else if (varnam(1:varlen).eq.'PINJALW') then
         if (isub1.ne.9999) goto 910
         oldval = pinjalw
         call getrv(nin,line,length,iptr,lineno,pinjalw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PINJALW',pinjalw,0.0D0,1.0D3)
         if (pinjalw.ne.oldval) then
            clabel = 'Maximum allowed injection power (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',pinjalw
         end if

C+**PJK 10/11/92 Removed redundant variable PINJIN
C+**PJK 10/11/92 Removed redundant variable PLHYBD0

      else if (varnam(1:varlen).eq.'TBEAMIN') then
         if (isub1.ne.9999) goto 910
         oldval = tbeamin
         call getrv(nin,line,length,iptr,lineno,tbeamin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TBEAMIN',tbeamin,0.0D0,10.0D0)
         if (tbeamin.ne.oldval) then
            clabel = 'No of NB decay lengths to plas centre,'
            write(nout,*) clabel,varnam(1:8),' = ',tbeamin
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block CDDAT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block CDDAT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL05(lineno,icode)

C  Handle the NAMELIST input data for block 'TIME'.
C
C  Reads data in NAMELIST format
C
C  $TIME
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/time/
c  +     tburn, tdwell, theat, tohs, tohsin, tqnch, tramp
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'times.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'TBURN') then
         if (isub1.ne.9999) goto 910
         oldval = tburn
         call getrv(nin,line,length,iptr,lineno,tburn,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TBURN',tburn,0.0D0,1.0D7)
         if (tburn.ne.oldval) then
            clabel = 'Burn time (s),'
            write(nout,*) clabel,varnam(1:8),' = ',tburn
         end if 
      elseif (varnam(1:varlen).eq.'TDWELL') then
         if (isub1.ne.9999) goto 910
         oldval = tdwell
         call getrv(nin,line,length,iptr,lineno,tdwell,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TDWELL',tdwell,0.0D0,1.0D4)
         if (tdwell.ne.oldval) then
            clabel = 'Time between burns (s),'
            write(nout,*) clabel,varnam(1:8),' = ',tdwell
         end if 
      else if (varnam(1:varlen).eq.'THEAT') then
         if (isub1.ne.9999) goto 910
         oldval = theat
         call getrv(nin,line,length,iptr,lineno,theat,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'THEAT',theat,0.0D0,1.0D4)
         if (theat.ne.oldval) then
            write(nout,*)
            clabel = 'Heating time after current ramp (s),'
            write(nout,*) clabel,varnam(1:8),' = ',theat
         end if
      else if (varnam(1:varlen).eq.'TOHS') then
         if (isub1.ne.9999) goto 910
         oldval = tohs
         call getrv(nin,line,length,iptr,lineno,tohs,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TOHS',tohs,0.0D0,1.0D4)
         if (tohs.ne.oldval) then
            clabel = 'OH coil swing time for current init (s),'
            write(nout,*) clabel,varnam(1:8),' = ',tohs
            write(nout,*) 'This variable need not be initialised...'
         end if
      else if (varnam(1:varlen).eq.'TOHSIN') then
         if (isub1.ne.9999) goto 910
         oldval = tohsin
         call getrv(nin,line,length,iptr,lineno,tohsin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TOHSIN',tohsin,0.0D0,1.0D4)
         if (tohsin.ne.oldval) then
            clabel = 'Switch for TOHS calculation,'
            write(nout,*) clabel,varnam(1:8),' = ',tohsin
            if (tohsin.eq.0.0d0) then
               write(nout,*) '     (tohs = Ip(MA)/0.5)'
            else
               write(nout,*) '     (tohs = tohsin (s) )'
            end if
         end if
      else if (varnam(1:varlen).eq.'TQNCH') then
         if (isub1.ne.9999) goto 910
         oldval = tqnch
         call getrv(nin,line,length,iptr,lineno,tqnch,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TQNCH',tqnch,0.0D0,1.0D4)
         if (tqnch.ne.oldval) then
            clabel = 'PF coil shutdown time (s),'
            write(nout,*) clabel,varnam(1:8),' = ',tqnch
         end if
      else if (varnam(1:varlen).eq.'TRAMP') then
         if (isub1.ne.9999) goto 910
         oldval = tramp
         call getrv(nin,line,length,iptr,lineno,tramp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TRAMP',tramp,0.0D0,1.0D4)
         if (tramp.ne.oldval) then
            clabel = 'Initial charge time for PF coils (s),'
            write(nout,*) clabel,varnam(1:8),' = ',tqnch
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block TIME'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block TIME'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL06(lineno,icode)

C  Handle the NAMELIST input data for block 'DIVT'.
C
C  Reads data in NAMELIST format
C
C  $DIVT
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/divt/
c  +     anginc, bpsout, c1div, c2div, c3div, c4div, c5div, c6div,
c  +     delld, divdum, fdfs, fgamp, fififi, frrp, hldivlim, ksic,
c  +     omegan, plsepo, prn1, rlenmax, xparain, fdiva, divdens,
c  +     divclfr, divplt, zeffdiv
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'divrt.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ANGINC') then
         if (isub1.ne.9999) goto 910
         oldval = anginc
         call getrv(nin,line,length,iptr,lineno,anginc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ANGINC',anginc,0.0D0,1.5707D0)
         if (anginc.ne.oldval) then
            clabel = 'Field line ang of incid on dvrtr (rad),'
            write(nout,*) clabel,varnam(1:8),' = ',anginc
         end if
      else if (varnam(1:varlen).eq.'BPSOUT') then
         if (isub1.ne.9999) goto 910
         oldval = bpsout
         call getrv(nin,line,length,iptr,lineno,bpsout,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BPSOUT',bpsout,0.0D0,10.0D0)
         if (bpsout.ne.oldval) then
            clabel = 'Ref B_p at outer divertor strike point,'
            write(nout,*) clabel,varnam(1:8),' = ',bpsout
         end if
      else if (varnam(1:varlen).eq.'C1DIV') then
         if (isub1.ne.9999) goto 910
         oldval = c1div
         call getrv(nin,line,length,iptr,lineno,c1div,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'C1DIV',c1div,-100.0D0,100.0D0)
         if (c1div.ne.oldval) then
            clabel = 'Divertor model fitting coefficient,'
            write(nout,*) clabel,varnam(1:8),' = ',c1div
         end if
      else if (varnam(1:varlen).eq.'C2DIV') then
         if (isub1.ne.9999) goto 910
         oldval = c2div
         call getrv(nin,line,length,iptr,lineno,c2div,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'C2DIV',c2div,-100.0D0,100.0D0)
         if (c2div.ne.oldval) then
            clabel = 'Divertor model fitting coefficient,'
            write(nout,*) clabel,varnam(1:8),' = ',c2div
         end if
      else if (varnam(1:varlen).eq.'C3DIV') then
         if (isub1.ne.9999) goto 910
         oldval = c3div
         call getrv(nin,line,length,iptr,lineno,c3div,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'C3DIV',c3div,-100.0D0,100.0D0)
         if (c3div.ne.oldval) then
            clabel = 'Divertor model fitting coefficient,'
            write(nout,*) clabel,varnam(1:8),' = ',c3div
         end if
      else if (varnam(1:varlen).eq.'C4DIV') then
         if (isub1.ne.9999) goto 910
         oldval = c4div
         call getrv(nin,line,length,iptr,lineno,c4div,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'C4DIV',c4div,-100.0D0,100.0D0)
         if (c4div.ne.oldval) then
            clabel = 'Divertor model fitting coefficient,'
            write(nout,*) clabel,varnam(1:8),' = ',c4div
         end if
      else if (varnam(1:varlen).eq.'C5DIV') then
         if (isub1.ne.9999) goto 910
         oldval = c5div
         call getrv(nin,line,length,iptr,lineno,c5div,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'C5DIV',c5div,-100.0D0,100.0D0)
         if (c5div.ne.oldval) then
            clabel = 'Divertor model fitting coefficient,'
            write(nout,*) clabel,varnam(1:8),' = ',c5div
         end if
      else if (varnam(1:varlen).eq.'C6DIV') then
         if (isub1.ne.9999) goto 910
         oldval = c6div
         call getrv(nin,line,length,iptr,lineno,c6div,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'C6DIV',c6div,-100.0D0,100.0D0)
         if (c6div.ne.oldval) then
            clabel = 'Divertor model fitting coefficient,'
            write(nout,*) clabel,varnam(1:8),' = ',c6div
         end if
      else if (varnam(1:varlen).eq.'DELLD') then
         if (isub1.ne.9999) goto 910
         oldval = delld
         call getrv(nin,line,length,iptr,lineno,delld,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DELLD',delld,0.1D0,2.0D0)
         if (delld.ne.oldval) then
            clabel = 'Coefficient for power distribution,'
            write(nout,*) clabel,varnam(1:8),' = ',delld
         end if
C *** PJK 25/04/02 Converted DIVDUM to integer
      else if (varnam(1:varlen).eq.'DIVDUM') then
         if (isub1.ne.9999) goto 910
         ioldvl = divdum
         call getiv(nin,line,length,iptr,lineno,divdum,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'DIVDUM',divdum,0,1)
         if (divdum.ne.ioldvl) then
            clabel = 'Switch for divertor Zeff value,'
            write(nout,*) clabel,varnam(1:8),' = ',divdum
            if (divdum.eq.0) then
               write(nout,*) '     (Calculated)'
            else
               write(nout,*) '     (Input via ZEFFDIV)'
            end if
         end if
      else if (varnam(1:varlen).eq.'FDFS') then
         if (isub1.ne.9999) goto 910
         oldval = fdfs
         call getrv(nin,line,length,iptr,lineno,fdfs,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FDFS',fdfs,0.0D0,20.0D0)
         if (fdfs.ne.oldval) then
            clabel = 'Radial gradient ratio,'
            write(nout,*) clabel,varnam(1:8),' = ',fdfs
         end if
      else if (varnam(1:varlen).eq.'FGAMP') then
         if (isub1.ne.9999) goto 910
         oldval = fgamp
         call getrv(nin,line,length,iptr,lineno,fgamp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FGAMP',fgamp,-100.0D0,100.0D0)
         if (fgamp.ne.oldval) then
            clabel = 'Sheath potential factor,'
            write(nout,*) clabel,varnam(1:8),' = ',fgamp
         end if
      else if (varnam(1:varlen).eq.'FIFIFI') then
         if (isub1.ne.9999) goto 910
         oldval = fififi
         call getrv(nin,line,length,iptr,lineno,fififi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FIFIFI',fififi,1.0D-6,1.0D0)
         if (fififi.ne.oldval) then
            clabel = 'Coefficient for gamdiv,'
            write(nout,*) clabel,varnam(1:8),' = ',fififi
         end if
      else if (varnam(1:varlen).eq.'FRRP') then
         if (isub1.ne.9999) goto 910
         oldval = frrp
         call getrv(nin,line,length,iptr,lineno,frrp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FRRP',frrp,0.0D0,1.0D0)
         if (frrp.ne.oldval) then
            clabel = 'Fraction of radiated power to plate,'
            write(nout,*) clabel,varnam(1:8),' = ',frrp
         end if
      else if (varnam(1:varlen).eq.'HLDIVLIM') then
         if (isub1.ne.9999) goto 910
         oldval = hldivlim
         call getrv(nin,line,length,iptr,lineno,hldivlim,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'HLDIVLIM',hldivlim,0.1D0,20.0D0)
         if (hldivlim.ne.oldval) then
            clabel = 'Divertor heat load limit (MW/m2),'
            write(nout,*) clabel,varnam(1:8),' = ',hldivlim
         end if
c  Note : ksic is declared as double precision
      else if (varnam(1:varlen).eq.'KSIC') then
         if (isub1.ne.9999) goto 910
         oldval = ksic
         call getrv(nin,line,length,iptr,lineno,ksic,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'KSIC',ksic,0.0D0,2.0D0)
         if (ksic.ne.oldval) then
            clabel = 'Divertor power fraction thingy,'
            write(nout,*) clabel,varnam(1:8),' = ',ksic
         end if
      else if (varnam(1:varlen).eq.'OMEGAN') then
         if (isub1.ne.9999) goto 910
         oldval = omegan
         call getrv(nin,line,length,iptr,lineno,omegan,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'OMEGAN',omegan,0.1D0,10.0D0)
         if (omegan.ne.oldval) then
            clabel = 'Pressure ratio (nT)_p / (nT)_s,'
            write(nout,*) clabel,varnam(1:8),' = ',omegan
         end if
      else if (varnam(1:varlen).eq.'PLSEPO') then
         if (isub1.ne.9999) goto 910
         oldval = plsepo
         call getrv(nin,line,length,iptr,lineno,plsepo,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PLSEPO',plsepo,0.1D0,10.0D0)
         if (plsepo.ne.oldval) then
            clabel = 'Poloidal length, x to outer strike point,'
            write(nout,*) clabel,varnam(1:8),' = ',plsepo
         end if
      else if (varnam(1:varlen).eq.'PRN1') then
         if (isub1.ne.9999) goto 910
         oldval = prn1
         call getrv(nin,line,length,iptr,lineno,prn1,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PRN1',prn1,0.0D0,1.0D0)
         if (prn1.ne.oldval) then
            clabel = 'n_scrapeoff / n_average plasma,'
            write(nout,*) clabel,varnam(1:8),' = ',prn1
         end if
      else if (varnam(1:varlen).eq.'RLENMAX') then
         if (isub1.ne.9999) goto 910
         oldval = rlenmax
         call getrv(nin,line,length,iptr,lineno,rlenmax,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RLENMAX',rlenmax,0.0D0,1.0D0)
         if (rlenmax.ne.oldval) then
            clabel = 'Maximum value for length ratio,'
            write(nout,*) clabel,varnam(1:8),' = ',rlenmax
         end if
      else if (varnam(1:varlen).eq.'XPARAIN') then
         if (isub1.ne.9999) goto 910
         oldval = xparain
         call getrv(nin,line,length,iptr,lineno,xparain,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'XPARAIN',xparain,0.01D0,1.0D4)
         if (xparain.ne.oldval) then
            clabel = 'Parallel heat transport coeff (m2/s),'
            write(nout,*) clabel,varnam(1:8),' = ',xparain
         end if
      else if (varnam(1:varlen).eq.'XPERTIN') then
         if (isub1.ne.9999) goto 910
         oldval = xpertin
         call getrv(nin,line,length,iptr,lineno,xpertin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'XPERTIN',xpertin,0.0D0,10.0D0)
         if (xpertin.ne.oldval) then
            clabel = 'Perpendicular heat trans coeff (m2/s),'
            write(nout,*) clabel,varnam(1:8),' = ',xpertin
         end if
      else if (varnam(1:varlen).eq.'FDIVA') then
         if (isub1.ne.9999) goto 910
         oldval = fdiva
         call getrv(nin,line,length,iptr,lineno,fdiva,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FDIVA',fdiva,0.1D0,2.0D0)
         if (fdiva.ne.oldval) then
            clabel = 'Divertor area fiddle factor,'
            write(nout,*) clabel,varnam(1:8),' = ',fdiva
         end if
      else if (varnam(1:varlen).eq.'DIVDENS') then
         if (isub1.ne.9999) goto 910
         oldval = divdens
         call getrv(nin,line,length,iptr,lineno,divdens,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DIVDENS',divdens,0.1D0,1.0D5)
         if (divdens.ne.oldval) then
            clabel = 'Divertor structure density (kg/m3),'
            write(nout,*) clabel,varnam(1:8),' = ',divdens
         end if
      else if (varnam(1:varlen).eq.'DIVCLFR') then
         if (isub1.ne.9999) goto 910
         oldval = divclfr
         call getrv(nin,line,length,iptr,lineno,divclfr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DIVCLFR',divclfr,0.0D0,1.0D0)
         if (divclfr.ne.oldval) then
            clabel = 'Divertor coolant fraction,'
            write(nout,*) clabel,varnam(1:8),' = ',divclfr
         end if
      else if (varnam(1:varlen).eq.'DIVPLT') then
         if (isub1.ne.9999) goto 910
         oldval = divplt
         call getrv(nin,line,length,iptr,lineno,divplt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DIVPLT',divplt,0.0D0,1.0D0)
         if (divplt.ne.oldval) then
            clabel = 'Divertor plate thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',divplt
         end if
C *** PJK 25/04/02
      else if (varnam(1:varlen).eq.'ZEFFDIV') then
         if (isub1.ne.9999) goto 910
         oldval = zeffdiv
         call getrv(nin,line,length,iptr,lineno,zeffdiv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ZEFFDIV',zeffdiv,0.01D0,100.0D0)
         if (zeffdiv.ne.oldval) then
            clabel = 'Zeff in the divertor region (if divdum.ne.0),'
            write(nout,*) clabel,varnam(1:8),' = ',zeffdiv
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block DIVT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block DIVT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL07(lineno,icode)

C  Handle the NAMELIST input data for block 'BLD'.
C
C  Reads data in NAMELIST format
C
C  $BLD
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/bld/
c  +     aplasmin, blnkith, blnkoth, bcylth, bore, ddwex, ddwi,
c  +     fmsbc, fmsbl, fmsdwe, fmsdwi, fmsfw, fmsoh, fmssh, fmstf,
c  +     gapoh, gapds, gapomin, fwith, fwoth, iohcl, ohcth, rinboard,
c  +     scrapli, scraplo, shldith, shldoth, shldtth, tfcth, tfootfi,
c  +     vgaptf, vgap2
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'APLASMIN') then
         if (isub1.ne.9999) goto 910
         oldval = aplasmin
         call getrv(nin,line,length,iptr,lineno,aplasmin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'APLASMIN',aplasmin,0.01D0,10.0D0)
         if (aplasmin.ne.oldval) then
            clabel = 'Minimum minor radius (m),'
            write(nout,*) clabel,varnam(1:8),' = ',aplasmin
         end if
      else if (varnam(1:varlen).eq.'BLNKITH') then
         if (isub1.ne.9999) goto 910
         oldval = blnkith
         call getrv(nin,line,length,iptr,lineno,blnkith,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BLNKITH',blnkith,0.0D0,10.0D0)
         if (blnkith.ne.oldval) then
            clabel = 'Inboard blanket thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',blnkith
         end if
      else if (varnam(1:varlen).eq.'BLNKOTH') then
         if (isub1.ne.9999) goto 910
         oldval = blnkoth
         call getrv(nin,line,length,iptr,lineno,blnkoth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BLNKOTH',blnkoth,0.0D0,10.0D0)
         if (blnkoth.ne.oldval) then
            clabel = 'Outboard blanket thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',blnkoth
         end if
      else if (varnam(1:varlen).eq.'BCYLTH') then
         if (isub1.ne.9999) goto 910
         oldval = bcylth
         call getrv(nin,line,length,iptr,lineno,bcylth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BCYLTH',bcylth,0.0D0,10.0D0)
         if (bcylth.ne.oldval) then
            clabel = 'Bucking cylinder thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',bcylth
         end if
      else if (varnam(1:varlen).eq.'BORE') then
         if (isub1.ne.9999) goto 910
         oldval = bore
         call getrv(nin,line,length,iptr,lineno,bore,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BORE',bore,0.0D0,20.0D0)
         if (bore.ne.oldval) then
            clabel = 'Machine bore (m),'
            write(nout,*) clabel,varnam(1:8),' = ',bore
         end if
      else if (varnam(1:varlen).eq.'DDWEX') then
         if (isub1.ne.9999) goto 910
         oldval = ddwex
         call getrv(nin,line,length,iptr,lineno,ddwex,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DDWEX',ddwex,0.0D0,10.0D0)
         if (ddwex.ne.oldval) then
            clabel = 'External dewar thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',ddwex
         end if
      else if (varnam(1:varlen).eq.'DDWI') then
         if (isub1.ne.9999) goto 910
         oldval = ddwi
         call getrv(nin,line,length,iptr,lineno,ddwi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DDWI',ddwi,0.0D0,10.0D0)
         if (ddwi.ne.oldval) then
            clabel = 'Dewar thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',ddwi
         end if
      else if (varnam(1:varlen).eq.'FMSBC') then
         if (isub1.ne.9999) goto 910
         oldval = fmsbc
         call getrv(nin,line,length,iptr,lineno,fmsbc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMSBC',fmsbc,0.0D0,1.0D0)
         if (fmsbc.ne.oldval) then
            clabel = 'Martensitic frac of steel in buck cyl,'
            write(nout,*) clabel,varnam(1:8),' = ',fmsbc
         end if
      else if (varnam(1:varlen).eq.'FMSBL') then
         if (isub1.ne.9999) goto 910
         oldval = fmsbl
         call getrv(nin,line,length,iptr,lineno,fmsbl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMSBL',fmsbl,0.0D0,1.0D0)
         if (fmsbl.ne.oldval) then
            clabel = 'Martensitic frac of steel in blanket,'
            write(nout,*) clabel,varnam(1:8),' = ',fmsbl
         end if
      else if (varnam(1:varlen).eq.'FMSDWE') then
         if (isub1.ne.9999) goto 910
         oldval = fmsdwe
         call getrv(nin,line,length,iptr,lineno,fmsdwe,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMSDWE',fmsdwe,0.0D0,1.0D0)
         if (fmsdwe.ne.oldval) then
            clabel = 'Martensitic frac of steel in external dewar,'
            write(nout,*) clabel,varnam(1:8),' = ',fmsdwe
         end if
      else if (varnam(1:varlen).eq.'FMSDWI') then
         if (isub1.ne.9999) goto 910
         oldval = fmsdwi
         call getrv(nin,line,length,iptr,lineno,fmsdwi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMSDWI',fmsdwi,0.0D0,1.0D0)
         if (fmsdwi.ne.oldval) then
            clabel = 'Martensitic frac of steel in dewar,'
            write(nout,*) clabel,varnam(1:8),' = ',fmsdwi
         end if
      else if (varnam(1:varlen).eq.'FMSFW') then
         if (isub1.ne.9999) goto 910
         oldval = fmsfw
         call getrv(nin,line,length,iptr,lineno,fmsfw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMSFW',fmsfw,0.0D0,1.0D0)
         if (fmsfw.ne.oldval) then
            clabel = 'Martensitic frac of steel in first wall,'
            write(nout,*) clabel,varnam(1:8),' = ',fmsfw
         end if
      else if (varnam(1:varlen).eq.'FMSOH') then
         if (isub1.ne.9999) goto 910
         oldval = fmsoh
         call getrv(nin,line,length,iptr,lineno,fmsoh,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMSOH',fmsoh,0.0D0,1.0D0)
         if (fmsoh.ne.oldval) then
            clabel = 'Martensitic frac of steel in OH coil,'
            write(nout,*) clabel,varnam(1:8),' = ',fmsoh
         end if
      else if (varnam(1:varlen).eq.'FMSSH') then
         if (isub1.ne.9999) goto 910
         oldval = fmssh
         call getrv(nin,line,length,iptr,lineno,fmssh,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMSSH',fmssh,0.0D0,1.0D0)
         if (fmssh.ne.oldval) then
            clabel = 'Martensitic frac of steel in shield,'
            write(nout,*) clabel,varnam(1:8),' = ',fmssh
         end if
      else if (varnam(1:varlen).eq.'FMSTF') then
         if (isub1.ne.9999) goto 910
         oldval = fmstf
         call getrv(nin,line,length,iptr,lineno,fmstf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMSTF',fmstf,0.0D0,1.0D0)
         if (fmstf.ne.oldval) then
            clabel = 'Martensitic frac of steel in TF coil,'
            write(nout,*) clabel,varnam(1:8),' = ',fmstf
         end if
      else if (varnam(1:varlen).eq.'FWITH') then
         if (isub1.ne.9999) goto 910
         oldval = fwith
         call getrv(nin,line,length,iptr,lineno,fwith,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FWITH',fwith,0.0D0,10.0D0)
         if (fwith.ne.oldval) then
            clabel = 'Inboard first wall thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',fwith
         end if
      else if (varnam(1:varlen).eq.'FWOTH') then
         if (isub1.ne.9999) goto 910
         oldval = fwoth
         call getrv(nin,line,length,iptr,lineno,fwoth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FWOTH',fwoth,0.0D0,10.0D0)
         if (fwoth.ne.oldval) then
            clabel = 'Outboard first wall thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',fwoth
         end if
      else if (varnam(1:varlen).eq.'GAPOH') then
         if (isub1.ne.9999) goto 910
         oldval = gapoh
         call getrv(nin,line,length,iptr,lineno,gapoh,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'GAPOH',gapoh,0.0D0,10.0D0)
         if (gapoh.ne.oldval) then
            clabel = 'Gap between OHC and bucking cylinder (m),'
            write(nout,*) clabel,varnam(1:8),' = ',gapoh
         end if
      else if (varnam(1:varlen).eq.'GAPDS') then
         if (isub1.ne.9999) goto 910
         oldval = gapds
         call getrv(nin,line,length,iptr,lineno,gapds,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'GAPDS',gapds,0.0D0,10.0D0)
         if (gapds.ne.oldval) then
            clabel = 'Gap between dewar and shield (m),'
            write(nout,*) clabel,varnam(1:8),' = ',gapds
         end if
      else if (varnam(1:varlen).eq.'GAPOMIN') then
         if (isub1.ne.9999) goto 910
         oldval = gapomin
         call getrv(nin,line,length,iptr,lineno,gapomin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'GAPOMIN',gapomin,0.0D0,10.0D0)
         if (gapomin.ne.oldval) then
            clabel = 'Min gap between outer shield & TFC (m),'
            write(nout,*) clabel,varnam(1:8),' = ',gapomin
         end if

C+**PJK 09/12/93 Removed obsolete variable IOHCIE

      else if (varnam(1:varlen).eq.'IOHCL') then
         if (isub1.ne.9999) goto 910
         ioldvl = iohcl
         call getiv(nin,line,length,iptr,lineno,iohcl,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IOHCL',iohcl,0,1)
         if (iohcl.ne.ioldvl) then
            clabel = 'Switch for existence of OH coil,'
            write(nout,*) clabel,varnam(1:8),' = ',iohcl
            if (iohcl.eq.0) then
               write(nout,*) '     (OH coil not present)'
            else
               write(nout,*) '     (OH coil present)'
            end if
         end if
      else if (varnam(1:varlen).eq.'OHCTH') then
         if (isub1.ne.9999) goto 910
         oldval = ohcth
         call getrv(nin,line,length,iptr,lineno,ohcth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'OHCTH',ohcth,0.0D0,10.0D0)
         if (ohcth.ne.oldval) then
            clabel = 'OH coil thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',ohcth
            if ((iohcl.eq.0).and.(ohcth.ne.0.0D0)) then
               write(nout,*)
     +              'Error - set IOHCL=1 before assigning OHCTH <> 0'
               write(nout,*) 'PROCESS stopping.'
               STOP
            end if
            if ((iohcl.eq.1).and.(ohcth.eq.0.0D0)) then
               write(nout,*)
     +              'Error - set IOHCL=0 before assigning OHCTH = 0'
               write(nout,*) 'PROCESS stopping.'
               STOP
            end if
         end if
      else if (varnam(1:varlen).eq.'RINBOARD') then
         if (isub1.ne.9999) goto 910
         oldval = rinboard
         call getrv(nin,line,length,iptr,lineno,rinboard,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RINBOARD',rinboard,0.0D0,10.0D0)
         if (rinboard.ne.oldval) then
            clabel = 'Plasma inboard radius (m),'
            write(nout,*) clabel,varnam(1:8),' = ',rinboard
         end if
      else if (varnam(1:varlen).eq.'SCRAPLI') then
         if (isub1.ne.9999) goto 910
         oldval = scrapli
         call getrv(nin,line,length,iptr,lineno,scrapli,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SCRAPLI',scrapli,0.0D0,10.0D0)
         if (scrapli.ne.oldval) then
            clabel = 'Inboard scrapeoff length (m),'
            write(nout,*) clabel,varnam(1:8),' = ',scrapli
         end if
      else if (varnam(1:varlen).eq.'SCRAPLO') then
         if (isub1.ne.9999) goto 910
         oldval = scraplo
         call getrv(nin,line,length,iptr,lineno,scraplo,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SCRAPLO',scraplo,0.0D0,10.0D0)
         if (scraplo.ne.oldval) then
            clabel = 'Outboard scrapeoff length (m),'
            write(nout,*) clabel,varnam(1:8),' = ',scraplo
         end if
      else if (varnam(1:varlen).eq.'SHLDITH') then
         if (isub1.ne.9999) goto 910
         oldval = shldith
         call getrv(nin,line,length,iptr,lineno,shldith,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SHLDITH',shldith,0.0D0,10.0D0)
         if (shldith.ne.oldval) then
            clabel = 'Inboard shield thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',shldith
         end if
      else if (varnam(1:varlen).eq.'SHLDOTH') then
         if (isub1.ne.9999) goto 910
         oldval = shldoth
         call getrv(nin,line,length,iptr,lineno,shldoth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SHLDOTH',shldoth,0.0D0,10.0D0)
         if (shldoth.ne.oldval) then
            clabel = 'Outboard shield thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',shldoth
         end if
      else if (varnam(1:varlen).eq.'SHLDTTH') then
         if (isub1.ne.9999) goto 910
         oldval = shldtth
         call getrv(nin,line,length,iptr,lineno,shldtth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SHLDTTH',shldtth,0.0D0,10.0D0)
         if (shldtth.ne.oldval) then
            clabel = 'Top shield thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',shldtth
         end if
      else if (varnam(1:varlen).eq.'TFCTH') then
         if (isub1.ne.9999) goto 910
         oldval = tfcth
         call getrv(nin,line,length,iptr,lineno,tfcth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TFCTH',tfcth,0.0D0,10.0D0)
         if (tfcth.ne.oldval) then
            clabel = 'TF coil thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',tfcth
         end if
      else if (varnam(1:varlen).eq.'TFOOTFI') then
         if (isub1.ne.9999) goto 910
         oldval = tfootfi
         call getrv(nin,line,length,iptr,lineno,tfootfi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TFOOTFI',tfootfi,0.2D0,5.0D0)
         if (tfootfi.ne.oldval) then
            clabel = 'TFC outboard/inboard leg thickness,'
            write(nout,*) clabel,varnam(1:8),' = ',tfootfi
         end if
      else if (varnam(1:varlen).eq.'VGAPTF') then
         if (isub1.ne.9999) goto 910
         oldval = vgaptf
         call getrv(nin,line,length,iptr,lineno,vgaptf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'VGAPTF',vgaptf,0.0D0,10.0D0)
         if (vgaptf.ne.oldval) then
            clabel = 'Vert gap between x-pnt and divertor (m),'
            write(nout,*) clabel,varnam(1:8),' = ',vgaptf
            if (vgaptf.eq.0.0d0) then
               write(nout,*) '     (This value is calculated)'
            end if
         end if
      else if (varnam(1:varlen).eq.'VGAP2') then
         if (isub1.ne.9999) goto 910
         oldval = vgap2
         call getrv(nin,line,length,iptr,lineno,vgap2,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'VGAP2',vgap2,0.0D0,10.0D0)
         if (vgap2.ne.oldval) then
            clabel = 'Vert gap between TF coil and shield (m),'
            write(nout,*) clabel,varnam(1:8),' = ',vgap2
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block BLD'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block BLD'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL08(lineno,icode)

C  Handle the NAMELIST input data for block 'TFC'.
C
C  Reads data in NAMELIST format
C
C  $TFC
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/tfc/
c  +     aspcstf, bcritsc, casfact, casthi, casths, cdtfleg, cpttf,
c  +     csutf, csytf, dcase, dcond, dcopper, drtop, dztop, etapump,
c  +     eystl, eyins, eywp, farc4tf, fcoolcp, fcutfsu, frhocp,
c  +     isumattf, itfmod, itfsup, jbus, jcrit_model, jcritsc, magnt,
c  +     oacdcp, poisson, ptempalw, rcool, ripmax, strncon, tcoolin, tcpav,
c  +     tcritsc, tdmptf, tflegres, tfno, tftmp, tftort, thicndut,
c  +     thkcas, thwcndut, tinstf, tmargmin, tmaxpro, tmpcry, vcool,
c  +     vdalw, vftf, wpvf
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'phydat.h'
      INCLUDE 'rfp.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ASPCSTF') then
         if (isub1.ne.9999) goto 910
         oldval = aspcstf
         call getrv(nin,line,length,iptr,lineno,aspcstf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ASPCSTF',aspcstf,0.1D0,10.0D0)
         if (aspcstf.ne.oldval) then
            clabel = 'TF conductor cable aspect ratio,'
            write(nout,*) clabel,varnam(1:8),' = ',aspcstf
         end if
      else if (varnam(1:varlen).eq.'BCRITSC') then
         if (isub1.ne.9999) goto 910
         oldval = bcritsc
         call getrv(nin,line,length,iptr,lineno,bcritsc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BCRITSC',bcritsc,10.0D0,50.0D0)
         if (bcritsc.ne.oldval) then
            clabel = 'Critical field for superconductor,'
            write(nout,*) clabel,varnam(1:8),' = ',bcritsc
         end if
      else if (varnam(1:varlen).eq.'CASFACT') then
         if (isub1.ne.9999) goto 910
         oldval = casfact
         call getrv(nin,line,length,iptr,lineno,casfact,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CASFACT',casfact,0.1D0,10.0D0)
         if (casfact.ne.oldval) then
            clabel = 'TF coil case thickness factor,'
            write(nout,*) clabel,varnam(1:8),' = ',casfact
         end if
      else if (varnam(1:varlen).eq.'CASTHI') then
         if (isub1.ne.9999) goto 910
         oldval = casthi
         call getrv(nin,line,length,iptr,lineno,casthi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CASTHI',casthi,0.0D0,1.0D0)
         if (casthi.ne.oldval) then
            clabel = 'TF coil case inner thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',casthi
         end if
      else if (varnam(1:varlen).eq.'CASTHS') then
         if (isub1.ne.9999) goto 910
         oldval = casths
         call getrv(nin,line,length,iptr,lineno,casths,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CASTHS',casths,0.0D0,1.0D0)
         if (casths.ne.oldval) then
            clabel = 'TF coil case sidewall thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',casths
         end if
      else if (varnam(1:varlen).eq.'CDTFLEG') then
         if (isub1.ne.9999) goto 910
         oldval = cdtfleg
         call getrv(nin,line,length,iptr,lineno,cdtfleg,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CDTFLEG',cdtfleg,0.1D0,1.0D8)
         if (cdtfleg.ne.oldval) then
            clabel = 'TF leg overall current density (A/m2),'
            write(nout,*) clabel,varnam(1:8),' = ',cdtfleg
         end if

C+**PJK 26/01/93 Removed redundant variable CPRES

      else if (varnam(1:varlen).eq.'CPTTF') then
         if (isub1.ne.9999) goto 910
         oldval = cpttf
         call getrv(nin,line,length,iptr,lineno,cpttf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CPTTF',cpttf,1.0D0,1.0D6)
         if (cpttf.ne.oldval) then
            clabel = 'TF coil leg current per turn (A),'
            write(nout,*) clabel,varnam(1:8),' = ',cpttf
         end if
      else if (varnam(1:varlen).eq.'CSUTF') then
         if (isub1.ne.9999) goto 910
         oldval = csutf
         call getrv(nin,line,length,iptr,lineno,csutf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CSUTF',csutf,1.0D6,1.0D11)
         if (csutf.ne.oldval) then
            clabel = 'Ultimate strength of TF coil case (Pa),'
            write(nout,*) clabel,varnam(1:8),' = ',csutf
         end if
      else if (varnam(1:varlen).eq.'CSYTF') then
         if (isub1.ne.9999) goto 910
         oldval = csytf
         call getrv(nin,line,length,iptr,lineno,csytf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CSYTF',csytf,1.0D6,1.0D11)
         if (csytf.ne.oldval) then
            clabel = 'Yield strength of TF coil case (Pa),'
            write(nout,*) clabel,varnam(1:8),' = ',csytf
         end if
      else if (varnam(1:varlen).eq.'DCASE') then
         if (isub1.ne.9999) goto 910
         oldval = dcase
         call getrv(nin,line,length,iptr,lineno,dcase,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DCASE',dcase,1.0D3,1.0D5)
         if (dcase.ne.oldval) then
            clabel = 'Density of TF coil case (kg/m3),'
            write(nout,*) clabel,varnam(1:8),' = ',dcase
         end if
      else if (varnam(1:varlen).eq.'DCOND') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'DCOND',rval,1.0D3,1.0D5)
            dcond(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 100        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'DCOND',rval,1.0D3,1.0D5)
            dcond(isub1) = rval
            isub1 = isub1 + 1
            goto 100
         end if
      else if (varnam(1:varlen).eq.'DCOPPER') then
         if (isub1.ne.9999) goto 910
         oldval = dcopper
         call getrv(nin,line,length,iptr,lineno,dcopper,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DCOPPER',dcopper,8.0D3,1.0D4)
         if (dcopper.ne.oldval) then
            clabel = 'Density of copper (kg/m3),'
            write(nout,*) clabel,varnam(1:8),' = ',dcopper
         end if
C+**PJK 18/11/97
      else if (varnam(1:varlen).eq.'DRTOP') then
         if (isub1.ne.9999) goto 910
         oldval = drtop
         call getrv(nin,line,length,iptr,lineno,drtop,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DRTOP',drtop,-1.5D0,1.5D0)
         if (drtop.ne.oldval) then
            clabel = 'ST CP top radius adjust (m),'
            write(nout,*) clabel,varnam(1:8),' = ',drtop
         end if
      else if (varnam(1:varlen).eq.'DZTOP') then
         if (isub1.ne.9999) goto 910
         oldval = dztop
         call getrv(nin,line,length,iptr,lineno,dztop,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DZTOP',dztop,-0.5D0,0.5D0)
         if (dztop.ne.oldval) then
            clabel = 'ST CP taper height adjust (m),'
            write(nout,*) clabel,varnam(1:8),' = ',dztop
         end if
C-**PJK 18/11/97
      else if (varnam(1:varlen).eq.'ETAPUMP') then
         if (isub1.ne.9999) goto 910
         oldval = etapump
         call getrv(nin,line,length,iptr,lineno,etapump,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAPUMP',etapump,0.0D0,1.0D0)
         if (etapump.ne.oldval) then
            clabel = 'Efficiency of c/p coolant pump,'
            write(nout,*) clabel,varnam(1:8),' = ',etapump
         end if
      else if (varnam(1:varlen).eq.'EYSTL') then
         if (isub1.ne.9999) goto 910
         oldval = eystl
         call getrv(nin,line,length,iptr,lineno,eystl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'EYSTL',eystl,1.0D8,1.0D13)
         if (eystl.ne.oldval) then
            clabel = 'Steel case Youngs Modulus (Pa),'
            write(nout,*) clabel,varnam(1:8),' = ',eystl
         end if
      else if (varnam(1:varlen).eq.'EYINS') then
         if (isub1.ne.9999) goto 910
         oldval = eyins
         call getrv(nin,line,length,iptr,lineno,eyins,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'EYINS',eyins,1.0D8,1.0D13)
         if (eyins.ne.oldval) then
            clabel = 'Insulator Youngs Modulus (Pa),'
            write(nout,*) clabel,varnam(1:8),' = ',eyins
         end if
      else if (varnam(1:varlen).eq.'EYWP') then
         if (isub1.ne.9999) goto 910
         oldval = eywp
         call getrv(nin,line,length,iptr,lineno,eywp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'EYWP',eywp,1.0D8,1.0D13)
         if (eywp.ne.oldval) then
            clabel = 'Winding pack Youngs Modulus (Pa),'
            write(nout,*) clabel,varnam(1:8),' = ',eywp
         end if
      else if (varnam(1:varlen).eq.'FARC4TF') then
         if (isub1.ne.9999) goto 910
         oldval = farc4tf
         call getrv(nin,line,length,iptr,lineno,farc4tf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FARC4TF',farc4tf,0.0D0,1.0D0)
         if (farc4tf.ne.oldval) then
            clabel = 'TF coil shape parameter,'
            write(nout,*) clabel,varnam(1:8),' = ',farc4tf
         end if
      else if (varnam(1:varlen).eq.'FCOOLCP') then
         if (isub1.ne.9999) goto 910
         oldval = fcoolcp
         call getrv(nin,line,length,iptr,lineno,fcoolcp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCOOLCP',fcoolcp,0.0D0,1.0D0)
         if (fcoolcp.ne.oldval) then
            clabel = 'Coolant fraction of TF inner leg,'
            write(nout,*) clabel,varnam(1:8),' = ',fcoolcp
         end if
      else if (varnam(1:varlen).eq.'FCUTFSU') then
         if (isub1.ne.9999) goto 910
         oldval = fcutfsu
         call getrv(nin,line,length,iptr,lineno,fcutfsu,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCUTFSU',fcutfsu,0.0D0,1.0D0)
         if (fcutfsu.ne.oldval) then
            clabel = 'Cu fraction of SCTF cable conductor,'
            write(nout,*) clabel,varnam(1:8),' = ',fcutfsu
         end if
C+**PJK 31/01/96
      else if (varnam(1:varlen).eq.'FRHOCP') then
         if (isub1.ne.9999) goto 910
         oldval = frhocp
         call getrv(nin,line,length,iptr,lineno,frhocp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FRHOCP',frhocp,0.01D0,5.0D0)
         if (frhocp.ne.oldval) then
            clabel = 'TART c/p resistivity enhancement factor,'
            write(nout,*) clabel,varnam(1:8),' = ',frhocp
         end if
      else if (varnam(1:varlen).eq.'ISUMATTF') then
         if (isub1.ne.9999) goto 910
         ioldvl = isumattf
         call getiv(nin,line,length,iptr,lineno,isumattf,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ISUMATTF',isumattf,1,5)
         if (isumattf.ne.ioldvl) then
            clabel = 'TF coil superconductor material,'
            write(nout,*) clabel,varnam(1:8),' = ',isumattf
            if (isumattf.eq.1) then
               write(nout,*) '     (binary Nb3Sn)'
            else if (isumattf.eq.2) then
               write(nout,*) '     (ternary Nb3Sn)'
            else if (isumattf.eq.3) then
               write(nout,*) '     (NbTi)'
            else
               write(nout,*) '     (generic)'
            end if
         end if
C+**PJK 26/07/11
      else if (varnam(1:varlen).eq.'JCRIT_MODEL') then
         if (isub1.ne.9999) goto 910
         ioldvl = jcrit_model
         call getiv(nin,line,length,iptr,lineno,jcrit_model,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'JCRIT_MODEL',jcrit_model,0,1)
         if (jcrit_model.ne.ioldvl) then
            clabel = 'Critical J model for binary Nb3Sn,'
            write(nout,*) clabel,varnam(1:8),' = ',jcrit_model
            if (jcrit_model.eq.0) then
               write(nout,*) '     (original model)'
            else
               write(nout,*) '     (ITER critical surface model)'
            end if
         end if
      else if (varnam(1:varlen).eq.'ITFMOD') then
         if (isub1.ne.9999) goto 910
         ioldvl = itfmod
         call getiv(nin,line,length,iptr,lineno,itfmod,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ITFMOD',itfmod,0,1)
         if (itfmod.ne.ioldvl) then
            clabel = 'Switch for TF magnet model,'
            write(nout,*) clabel,varnam(1:8),' = ',itfmod
            if (itfmod.eq.0) then
               write(nout,*) '     (Simple model)'
            else
               write(nout,*)
     +              '     (Complex stress/superconductor model)'
            end if
         end if
      else if (varnam(1:varlen).eq.'ITFSUP') then
         if (isub1.ne.9999) goto 910
         ioldvl = itfsup
         call getiv(nin,line,length,iptr,lineno,itfsup,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ITFSUP',itfsup,0,1)
         if (itfsup.ne.ioldvl) then
            clabel = 'Switch for TF coil type,'
            write(nout,*) clabel,varnam(1:8),' = ',itfsup
            if (itfsup.eq.1) then
               write(nout,*) '     (Superconductor)'
            else
               write(nout,*) '     (Conventional copper)'
            end if
         end if
      else if (varnam(1:varlen).eq.'JBUS') then
         if (isub1.ne.9999) goto 910
         oldval = jbus
         call getrv(nin,line,length,iptr,lineno,jbus,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'JBUS',jbus,1.0D4,1.0D8)
         if (jbus.ne.oldval) then
            clabel = 'TF coil bus current density (A/m2),'
            write(nout,*) clabel,varnam(1:8),' = ',jbus
         end if
      else if (varnam(1:varlen).eq.'JCRITSC') then
         if (isub1.ne.9999) goto 910
         oldval = JCRITSC
         call getrv(nin,line,length,iptr,lineno,JCRITSC,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'JCRITSC',JCRITSC,1.0D9,5.0D11)
         if (JCRITSC.ne.oldval) then
            clabel = 'Critical J for superconductor,'
            write(nout,*) clabel,varnam(1:8),' = ',JCRITSC
         end if
      else if (varnam(1:varlen).eq.'MAGNT') then
         if (isub1.ne.9999) goto 910
         ioldvl = magnt
         call getiv(nin,line,length,iptr,lineno,magnt,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'MAGNT',magnt,1,3)
         if (magnt.ne.ioldvl) then
            clabel = 'SCTF coil stress model,'
            write(nout,*) clabel,varnam(1:8),' = ',magnt
            if (magnt.eq.1) then
               write(nout,*)
     +           '     (FER type configuration with bucking cylinder)'
            else if (magnt.eq.2) then
               write(nout,*) '     (NET type wedging)'
            else
               write(nout,*) '     (LLNL type buck on OH coil)'
            end if
         end if
      else if (varnam(1:varlen).eq.'OACDCP') then
         if (isub1.ne.9999) goto 910
         oldval = oacdcp
         call getrv(nin,line,length,iptr,lineno,oacdcp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'OACDCP',oacdcp,1.0D4,1.0D9)
         if (oacdcp.ne.oldval) then
            clabel = 'Overall J in inboard TF coil midplane,'
            write(nout,*) clabel,varnam(1:8),' = ',oacdcp
         end if
      else if (varnam(1:varlen).eq.'POISSON') then
         if (isub1.ne.9999) goto 910
         oldval = poisson
         call getrv(nin,line,length,iptr,lineno,poisson,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'POISSON',poisson,0.0D0,1.0D0)
         if (poisson.ne.oldval) then
            clabel = 'Poissons ratio for TF stress calc.,'
            write(nout,*) clabel,varnam(1:8),' = ',poisson
         end if
C+**PJK 17/11/97
      else if (varnam(1:varlen).eq.'PTEMPALW') then
         if (isub1.ne.9999) goto 910
         oldval = ptempalw
         call getrv(nin,line,length,iptr,lineno,ptempalw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PTEMPALW',ptempalw,50.0D0,300.0D0)
         if (ptempalw.ne.oldval) then
            clabel = 'Maximum peak centrepost temp. (C),'
            write(nout,*) clabel,varnam(1:8),' = ',ptempalw
         end if
C-**PJK 17/11/97
      else if (varnam(1:varlen).eq.'RCOOL') then
         if (isub1.ne.9999) goto 910
         oldval = rcool
         call getrv(nin,line,length,iptr,lineno,rcool,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RCOOL',rcool,1.0D-6,1.0D0)
         if (rcool.ne.oldval) then
            clabel = 'Centrepost coolant channel radius,'
            write(nout,*) clabel,varnam(1:8),' = ',rcool
         end if
      else if (varnam(1:varlen).eq.'RIPMAX') then
         if (isub1.ne.9999) goto 910
         oldval = ripmax
         call getrv(nin,line,length,iptr,lineno,ripmax,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RIPMAX',ripmax,0.1D0,100.0D0)
         if (ripmax.ne.oldval) then
            clabel = 'Max peak/ave ripple at plasma edge (%),'
            write(nout,*) clabel,varnam(1:8),' = ',ripmax
         end if
      else if (varnam(1:varlen).eq.'STRNCON') then
         if (isub1.ne.9999) goto 910
         oldval = strncon
         call getrv(nin,line,length,iptr,lineno,strncon,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'STRNCON',strncon,-1.0D0,1.0D0)
         if (strncon.ne.oldval) then
            clabel = 'Strain in superconductor material,'
            write(nout,*) clabel,varnam(1:8),' = ',strncon
         end if
      else if (varnam(1:varlen).eq.'TCOOLIN') then
         if (isub1.ne.9999) goto 910
         oldval = tcoolin
         call getrv(nin,line,length,iptr,lineno,tcoolin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TCOOLIN',tcoolin,-273.15D0,100.0D0)
         if (tcoolin.ne.oldval) then
            clabel = 'Centrepost coolant inlet temperature,'
            write(nout,*) clabel,varnam(1:8),' = ',tcoolin
         end if
C+**PJK 31/01/96
      else if (varnam(1:varlen).eq.'TCPAV') then
         if (isub1.ne.9999) goto 910
         oldval = tcpav
         call getrv(nin,line,length,iptr,lineno,tcpav,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TCPAV',tcpav,-200.0D0,300.0D0)
         if (tcpav.ne.oldval) then
            clabel = 'Average centrepost coolant temperature,'
            write(nout,*) clabel,varnam(1:8),' = ',tcpav
         end if
      else if (varnam(1:varlen).eq.'TCRITSC') then
         if (isub1.ne.9999) goto 910
         oldval = tcritsc
         call getrv(nin,line,length,iptr,lineno,tcritsc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TCRITSC',tcritsc,1.0D0,300.0D0)
         if (tcritsc.ne.oldval) then
            clabel = 'Critical temperature for superconductor,'
            write(nout,*) clabel,varnam(1:8),' = ',tcritsc
         end if
      else if (varnam(1:varlen).eq.'TDMPTF') then
         if (isub1.ne.9999) goto 910
         oldval = tdmptf
         call getrv(nin,line,length,iptr,lineno,tdmptf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TDMPTF',tdmptf,0.0D0,1.0D2)
         if (tdmptf.ne.oldval) then
            clabel = 'Dump time for TF coil (s),'
            write(nout,*) clabel,varnam(1:8),' = ',tdmptf
         end if
      else if (varnam(1:varlen).eq.'TFLEGRES') then
         if (isub1.ne.9999) goto 910
         oldval = tflegres
         call getrv(nin,line,length,iptr,lineno,tflegres,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TFLEGRES',tflegres,1.0D-10,1.0D-5)
         if (tflegres.ne.oldval) then
            clabel = 'TF coil leg resistivity (ohm-m),'
            write(nout,*) clabel,varnam(1:8),' = ',tflegres
         end if
      else if (varnam(1:varlen).eq.'TFNO') then
         if (isub1.ne.9999) goto 910
         oldval = tfno
         call getrv(nin,line,length,iptr,lineno,tfno,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TFNO',tfno,0.0D0,1.0D2)
         if (tfno.ne.oldval) then
            clabel = 'Number of tf coils,'
            write(nout,*) clabel,varnam(1:8),' = ',tfno
         end if
      else if (varnam(1:varlen).eq.'TFTMP') then
         if (isub1.ne.9999) goto 910
         oldval = tftmp
         call getrv(nin,line,length,iptr,lineno,tftmp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TFTMP',tftmp,0.01D0,10.0D0)
         if (tftmp.ne.oldval) then
            clabel = 'Peak TF coil He coolant temp. (K),'
            write(nout,*) clabel,varnam(1:8),' = ',tftmp
         end if
C+**PJK 28/02/96
      else if (varnam(1:varlen).eq.'TFTORT') then
         if (isub1.ne.9999) goto 910
         oldval = tftort
         call getrv(nin,line,length,iptr,lineno,tftort,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TFTORT',tftort,0.1D0,2.0D0)
         if (tftort.ne.oldval) then
            clabel = 'TF coil toroidal thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',tftort
         end if
      else if (varnam(1:varlen).eq.'THICNDUT') then
         if (isub1.ne.9999) goto 910
         oldval = thicndut
         call getrv(nin,line,length,iptr,lineno,thicndut,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'THICNDUT',thicndut,0.0D0,0.1D0)
         if (thicndut.ne.oldval) then
            clabel = 'Conduit insulation thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',thicndut
         end if
      else if (varnam(1:varlen).eq.'THKCAS') then
         if (isub1.ne.9999) goto 910
         oldval = thkcas
         call getrv(nin,line,length,iptr,lineno,thkcas,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'THKCAS',thkcas,0.0D0,1.0D0)
         if (thkcas.ne.oldval) then
            clabel = 'External supercond. case thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',thkcas
         end if
      else if (varnam(1:varlen).eq.'THWCNDUT') then
         if (isub1.ne.9999) goto 910
         oldval = thwcndut
         call getrv(nin,line,length,iptr,lineno,thwcndut,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'THWCNDUT',thwcndut,0.0D0,0.1D0)
         if (thwcndut.ne.oldval) then
            clabel = 'TF coil conduit case thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',thwcndut
         end if
      else if (varnam(1:varlen).eq.'TINSTF') then
         if (isub1.ne.9999) goto 910
         oldval = tinstf
         call getrv(nin,line,length,iptr,lineno,tinstf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TINSTF',tinstf,0.0D0,0.1D0)
         if (tinstf.ne.oldval) then
            clabel = 'Ground wall insulation thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',tinstf
         end if
      else if (varnam(1:varlen).eq.'TMARGMIN') then
         if (isub1.ne.9999) goto 910
         oldval = tmargmin
         call getrv(nin,line,length,iptr,lineno,tmargmin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TMARGMIN',tmargmin,0.0D0,10.0D0)
         if (tmargmin.ne.oldval) then
            clabel = 'Minimum allowable temp margin (K),'
            write(nout,*) clabel,varnam(1:8),' = ',tmargmin
         end if
      else if (varnam(1:varlen).eq.'TMAXPRO') then
         if (isub1.ne.9999) goto 910
         oldval = tmaxpro
         call getrv(nin,line,length,iptr,lineno,tmaxpro,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TMAXPRO',tmaxpro,0.0D0,1.0D3)
         if (tmaxpro.ne.oldval) then
            clabel = 'Maximum temp rise during quench (K),'
            write(nout,*) clabel,varnam(1:8),' = ',tmaxpro
         end if
      else if (varnam(1:varlen).eq.'TMPCRY') then
         if (isub1.ne.9999) goto 910
         oldval = tmpcry
         call getrv(nin,line,length,iptr,lineno,tmpcry,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TMPCRY',tmpcry,0.01D0,10.0D0)
         if (tmpcry.ne.oldval) then
            clabel = 'Cryogenic temperature (K),'
            write(nout,*) clabel,varnam(1:8),' = ',tmpcry
         end if
      else if (varnam(1:varlen).eq.'VCOOL') then
         if (isub1.ne.9999) goto 910
         oldval = vcool
         call getrv(nin,line,length,iptr,lineno,vcool,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'VCOOL',vcool,0.001D0,1.0D2)
         if (vcool.ne.oldval) then
            clabel = 'Max centrepost coolant speed (m/s),'
            write(nout,*) clabel,varnam(1:8),' = ',vcool
         end if
      else if (varnam(1:varlen).eq.'VDALW') then
         if (isub1.ne.9999) goto 910
         oldval = vdalw
         call getrv(nin,line,length,iptr,lineno,vdalw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'VDALW',vdalw,0.0D0,1.0D2)
         if (vdalw.ne.oldval) then
            clabel = 'Max V across TFC during quench (kV),'
            write(nout,*) clabel,varnam(1:8),' = ',vdalw
         end if
      else if (varnam(1:varlen).eq.'VFTF') then
         if (isub1.ne.9999) goto 910
         oldval = vftf
         call getrv(nin,line,length,iptr,lineno,vftf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'VFTF',vftf,0.0D0,1.0D0)
         if (vftf.ne.oldval) then
            clabel = 'Coolant fraction of TF coil leg,'
            write(nout,*) clabel,varnam(1:8),' = ',vftf
         end if
C+**PJK 05/02/96
      else if (varnam(1:varlen).eq.'WPVF') then
         if (isub1.ne.9999) goto 910
         oldval = wpvf
         call getrv(nin,line,length,iptr,lineno,wpvf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'WPVF',wpvf,0.0D0,1.0D0)
         if (wpvf.ne.oldval) then
            clabel = 'Void fraction of TF coil winding pack,'
            write(nout,*) clabel,varnam(1:8),' = ',wpvf
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block TFC'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block TFC'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL09(lineno,icode)

C  Handle the NAMELIST input data for block 'PFC'.
C
C  Reads data in NAMELIST format
C
C  $PFC
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/pfc/
c  +     alfapf, coheof, cptdin, fcohbof, fcohbop, ipfloc, ipfres,
c  +     isumatpf, ncls, ngrp, ohhghf, pfclres, rjconpf, sigpfalw,
c  +     vf, vfohc, nfxfh, routr, rpf1, rpf2, sccufac, zref, fcuoh,
c  +     acsoh, ac1oh
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'phydat.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ALFAPF') then
         if (isub1.ne.9999) goto 910
         oldval = alfapf
         call getrv(nin,line,length,iptr,lineno,alfapf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ALFAPF',alfapf,1.0D-12,1.0D0)
         if (alfapf.ne.oldval) then
            clabel = 'PF coil current smoothing parameter,'
            write(nout,*) clabel,varnam(1:8),' = ',alfapf
         end if
      else if (varnam(1:varlen).eq.'COHEOF') then
         if (isub1.ne.9999) goto 910
         oldval = coheof
         call getrv(nin,line,length,iptr,lineno,coheof,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'COHEOF',coheof,1.0D4,1.0D8)
         if (coheof.ne.oldval) then
            clabel = 'OH coil current density at EOF,'
            write(nout,*) clabel,varnam(1:8),' = ',coheof
         end if
      else if (varnam(1:varlen).eq.'CPTDIN') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'CPTDIN',rval,1.0D2,1.0D6)
            cptdin(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 100        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'CPTDIN',rval,1.0D2,1.0D6)
            cptdin(isub1) = rval
            isub1 = isub1 + 1
            goto 100
         end if
      else if (varnam(1:varlen).eq.'FCOHBOF') then
         if (isub1.ne.9999) goto 910
         oldval = fcohbof
         call getrv(nin,line,length,iptr,lineno,fcohbof,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCOHBOF',fcohbof,0.0D0,1.0D0)
         if (fcohbof.ne.oldval) then
            clabel = 'OH coil J ratio : BOF/EOF,'
            write(nout,*) clabel,varnam(1:8),' = ',fcohbof
         end if
      else if (varnam(1:varlen).eq.'FCOHBOP') then
         if (isub1.ne.9999) goto 910
         oldval = fcohbop
         call getrv(nin,line,length,iptr,lineno,fcohbop,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCOHBOP',fcohbop,0.0D0,1.0D0)
         if (fcohbop.ne.oldval) then
            clabel = 'OH coil J ratio : BOP/EOF,'
            write(nout,*) clabel,varnam(1:8),' = ',fcohbop
         end if
      else if (varnam(1:varlen).eq.'IPFLOC') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getiv(nin,line,length,iptr,lineno,ival,icode)
            if (icode.ne.0) goto 900
            call rangei(nout,'IPFLOC',ival,1,3)
            ipfloc(isub1) = ival
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 110        continue
            call getiv(nin,line,length,iptr,lineno,ival,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call rangei(nout,'IPFLOC',ival,1,3)
            ipfloc(isub1) = ival
            isub1 = isub1 + 1
            goto 110
         end if
      else if (varnam(1:varlen).eq.'IPFRES') then
         if (isub1.ne.9999) goto 910
         ioldvl = ipfres
         call getiv(nin,line,length,iptr,lineno,ipfres,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IPFRES',ipfres,0,1)
         if (ipfres.ne.ioldvl) then
            clabel = 'Switch for supercond / resist PF coils,'
            write(nout,*) clabel,varnam(1:8),' = ',ipfres
            if (ipfres.eq.0) then
               write(nout,*) '     (PF coils are superconducting)'
            else
               write(nout,*) '     (PF coils are resistive)'
            end if
         end if
      else if (varnam(1:varlen).eq.'ISUMATPF') then
         if (isub1.ne.9999) goto 910
         ioldvl = isumatpf
         call getiv(nin,line,length,iptr,lineno,isumatpf,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ISUMATPF',isumatpf,1,3)
         if (isumatpf.ne.ioldvl) then
            clabel = 'PF coil superconductor material,'
            write(nout,*) clabel,varnam(1:8),' = ',isumatpf
            if (isumatpf.eq.1) then
               write(nout,*) '     (binary Nb3Sn)'
            else if (isumatpf.eq.2) then
               write(nout,*) '     (ternary Nb3Sn)'
            else
               write(nout,*) '     (NbTi)'
            end if
         end if

C+**PJK 09/12/93 Removed obsolete variable ISTOKPF

      else if (varnam(1:varlen).eq.'NCLS') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getiv(nin,line,length,iptr,lineno,ival,icode)
            if (icode.ne.0) goto 900
            call rangei(nout,'NCLS',ival,0,NCLSMX)
            ncls(isub1) = ival
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 120        continue
            call getiv(nin,line,length,iptr,lineno,ival,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call rangei(nout,'NCLS',ival,0,NCLSMX)
            ncls(isub1) = ival
            isub1 = isub1 + 1
            goto 120
         end if
      else if (varnam(1:varlen).eq.'NGRP') then
         if (isub1.ne.9999) goto 910
         ioldvl = ngrp
         call getiv(nin,line,length,iptr,lineno,ngrp,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'NGRP',ngrp,0,NGRPMX)
         if (ngrp.ne.ioldvl) then
            clabel = 'No of groups of PF coils,'
            write(nout,*) clabel,varnam(1:8),' = ',ngrp
         end if
      else if (varnam(1:varlen).eq.'OHHGHF') then
         if (isub1.ne.9999) goto 910
         oldval = ohhghf
         call getrv(nin,line,length,iptr,lineno,ohhghf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'OHHGHF',ohhghf,0.0D0,2.0D0)
         if (ohhghf.ne.oldval) then
            clabel = 'OH coil height / TF coil height,'
            write(nout,*) clabel,varnam(1:8),' = ',ohhghf
         end if
      else if (varnam(1:varlen).eq.'PFCLRES') then
         if (isub1.ne.9999) goto 910
         oldval = pfclres
         call getrv(nin,line,length,iptr,lineno,pfclres,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PFCLRES',pfclres,0.0D0,1.0D-4)
         if (pfclres.ne.oldval) then
            clabel = 'PF coil resistivity (ohm-m),'
            write(nout,*) clabel,varnam(1:8),' = ',pfclres
         end if

C+**PJK 11/11/92 Removed variables POWPFRES and POWOHRES:
C+**PJK 11/11/92 It is pointless to initialise them.

      else if (varnam(1:varlen).eq.'RJCONPF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'RJCONPF',rval,1.0D4,1.0D9)
            rjconpf(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 130        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'RJCONPF',rval,1.0D4,1.0D9)
            rjconpf(isub1) = rval
            isub1 = isub1 + 1
            goto 130
         end if
      else if (varnam(1:varlen).eq.'SIGPFALW') then
         if (isub1.ne.9999) goto 910
         oldval = sigpfalw
         call getrv(nin,line,length,iptr,lineno,sigpfalw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SIGPFALW',sigpfalw,1.0D0,1.0D3)
         if (sigpfalw.ne.oldval) then
            clabel = 'Allowable stress in the PF coil (MPa),'
            write(nout,*) clabel,varnam(1:8),' = ',sigpfalw
         end if
      else if (varnam(1:varlen).eq.'VF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'VF',rval,0.0D0,1.0D0)
            vf(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 140        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'VF',rval,0.0D0,1.0D0)
            vf(isub1) = rval
            isub1 = isub1 + 1
            goto 140
         end if
      else if (varnam(1:varlen).eq.'VFOHC') then
         if (isub1.ne.9999) goto 910
         oldval = vfohc
         call getrv(nin,line,length,iptr,lineno,vfohc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'VFOHC',vfohc,0.0D0,1.0D0)
         if (vfohc.ne.oldval) then
            clabel = 'OH coil void fraction for coolant,'
            write(nout,*) clabel,varnam(1:8),' = ',vfohc
         end if
      else if (varnam(1:varlen).eq.'NFXFH') then
         if (isub1.ne.9999) goto 910
         ioldvl = nfxfh
         call getiv(nin,line,length,iptr,lineno,nfxfh,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'NFXFH',nfxfh,1,NFIXMX/2)
         if (nfxfh.ne.ioldvl) then
            clabel = 'OH coil splitting parameter,'
            write(nout,*) clabel,varnam(1:8),' = ',nfxfh
         end if
      else if (varnam(1:varlen).eq.'ROUTR') then
         if (isub1.ne.9999) goto 910
         oldval = routr
         call getrv(nin,line,length,iptr,lineno,routr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ROUTR',routr,-3.0D0,3.0D0)
         if (routr.ne.oldval) then
            clabel = 'Gap from outer TFC leg for PFC,'
            write(nout,*) clabel,varnam(1:8),' = ',routr
         end if
      else if (varnam(1:varlen).eq.'RPF1') then
         if (isub1.ne.9999) goto 910
         oldval = rpf1
         call getrv(nin,line,length,iptr,lineno,rpf1,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RPF1',rpf1,0.0D0,3.0D0)
         if (rpf1.ne.oldval) then
            clabel = 'Radial offset for group 1 PF coils,'
            write(nout,*) clabel,varnam(1:8),' = ',rpf1
         end if
      else if (varnam(1:varlen).eq.'RPF2') then
         if (isub1.ne.9999) goto 910
         oldval = rpf2
         call getrv(nin,line,length,iptr,lineno,rpf2,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RPF2',rpf2,-3.0D0,3.0D0)
         if (rpf2.ne.oldval) then
            clabel = 'Radial offset for group 2 PF coils,'
            write(nout,*) clabel,varnam(1:8),' = ',rpf2
         end if
      else if (varnam(1:varlen).eq.'SCCUFAC') then
         if (isub1.ne.9999) goto 910
         oldval = sccufac
         call getrv(nin,line,length,iptr,lineno,sccufac,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SCCUFAC',sccufac,0.001D0,0.1D0)
         if (sccufac.ne.oldval) then
            clabel = 'sc/cu ratio in PF coils per tesla,'
            write(nout,*) clabel,varnam(1:8),' = ',sccufac
         end if
      else if (varnam(1:varlen).eq.'ZREF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'ZREF',rval,0.0D0,10.0D0)
            zref(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 150        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'ZREF',rval,0.0D0,10.0D0)
            zref(isub1) = rval
            isub1 = isub1+1
            goto 150
         end if
      else if (varnam(1:varlen).eq.'FCUOH') then
         if (isub1.ne.9999) goto 910
         oldval = fcuoh
         call getrv(nin,line,length,iptr,lineno,fcuoh,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCUOH',fcuoh,0.0D0,1.0D0)
         if (fcuoh.ne.oldval) then
            clabel = 'Cu frac of conductor in OH coil cable,'
            write(nout,*) clabel,varnam(1:8),' = ',fcuoh
         end if
      else if (varnam(1:varlen).eq.'ACSOH') then
         if (isub1.ne.9999) goto 910
         oldval = acsoh
         call getrv(nin,line,length,iptr,lineno,acsoh,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ACSOH',acsoh,1.0D-6,1.0D0)
         if (acsoh.ne.oldval) then
            clabel = 'Conduit conductor X-section (m2),'
            write(nout,*) clabel,varnam(1:8),' = ',acsoh
         end if
      else if (varnam(1:varlen).eq.'AC1OH') then
         if (isub1.ne.9999) goto 910
         oldval = ac1oh
         call getrv(nin,line,length,iptr,lineno,ac1oh,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'AC1OH',ac1oh,0.0D0,1.0D0)
         if (ac1oh.ne.oldval) then
            clabel = 'OH coil cable conduit area (m2),'
            write(nout,*) clabel,varnam(1:8),' = ',ac1oh
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block PFC'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block PFC'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL10(lineno,icode)

C  Handle the NAMELIST input data for block 'PULSE'.
C
C  Reads data in NAMELIST format
C
C  $PULSE
C  ...
C  $END
C
C  for the NAMELIST block:
C
C  namelist/pulse/
C +     afw,bctmp,coolp,dtstor,tmprse,istore,itcycl,lpulse
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'pulse.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'AFW') then
         if (isub1.ne.9999) goto 910
         oldval = afw
         call getrv(nin,line,length,iptr,lineno,afw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'AFW',afw,1.0D-3,0.5D0)
         if (afw.ne.oldval) then
            clabel = 'Inner radius of first wall coolant channel (m),'
            write(nout,*) clabel,varnam(1:8),' = ',afw
         end if

      else if (varnam(1:varlen).eq.'BCTMP') then
         if (isub1.ne.9999) goto 910
         oldval = bctmp
         call getrv(nin,line,length,iptr,lineno,bctmp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BCTMP',bctmp,1.0D0,800.0D0)
         if (bctmp.ne.oldval) then
            clabel = 'First wall bulk coolant temperature (C),'
            write(nout,*) clabel,varnam(1:8),' = ',bctmp
         end if

      else if (varnam(1:varlen).eq.'COOLP') then
         if (isub1.ne.9999) goto 910
         oldval = coolp
         call getrv(nin,line,length,iptr,lineno,coolp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'COOLP',coolp,1.0D5,1.0D9)
         if (coolp.ne.oldval) then
            clabel = 'First wall coolant pressure (Pa),'
            write(nout,*) clabel,varnam(1:8),' = ',coolp
         end if

      else if (varnam(1:varlen).eq.'DTSTOR') then
         if (isub1.ne.9999) goto 910
         oldval = dtstor
         call getrv(nin,line,length,iptr,lineno,dtstor,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DTSTOR',dtstor,50.0D0,500.0D0)
         if (dtstor.ne.oldval) then
            clabel = 'Max temp change in thermal storage medium (K),'
            write(nout,*) clabel,varnam(1:8),' = ',dtstor
         end if

      else if (varnam(1:varlen).eq.'TMPRSE') then
         if (isub1.ne.9999) goto 910
         oldval = tmprse
         call getrv(nin,line,length,iptr,lineno,tmprse,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TMPRSE',tmprse,1.0D0,1.0D3)
         if (tmprse.ne.oldval) then
            clabel = 'Temperature rise in first wall coolant (C),'
            write(nout,*) clabel,varnam(1:8),' = ',tmprse
         end if

      else if (varnam(1:varlen).eq.'ISTORE') then
         if (isub1.ne.9999) goto 910
         ioldvl = istore
         call getiv(nin,line,length,iptr,lineno,istore,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ISTORE',istore,1,3)
         if (istore.ne.ioldvl) then
            clabel = 'Switch for thermal storage option,'
            write(nout,*) clabel,varnam(1:8),' = ',istore
            if (istore.eq.1) then
               write(nout,*) '     (Option 1 of ELECTROWATT report)'
            else if (istore.eq.2) then
               write(nout,*) '     (Option 2 of ELECTROWATT report)'
            else
               write(nout,*) '     (Stainless steel block)'
            end if
         end if         

      else if (varnam(1:varlen).eq.'ITCYCL') then
         if (isub1.ne.9999) goto 910
         ioldvl = itcycl
         call getiv(nin,line,length,iptr,lineno,itcycl,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ITCYCL',itcycl,1,3)
         if (itcycl.ne.ioldvl) then
            clabel = 'Switch for 1st wall axial stress model,'
            write(nout,*) clabel,varnam(1:8),' = ',itcycl
            if (itcycl.eq.1) then
               write(nout,*) '     (Total axial restraint, no bending)'
            else if (itcycl.eq.2) then
               write(nout,*) '     (No axial restraint, no bending)'
            else
               write(nout,*) '     (No axial restraint, bending)'
            end if
         end if         

      else if (varnam(1:varlen).eq.'LPULSE') then
         if (isub1.ne.9999) goto 910
         ioldvl = lpulse
         call getiv(nin,line,length,iptr,lineno,lpulse,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'LPULSE',lpulse,0,1)
         if (lpulse.ne.ioldvl) then
            clabel = 'Switch for pulsed reactor model,'
            write(nout,*) clabel,varnam(1:8),' = ',lpulse
            if (lpulse.eq.1) then
               write(nout,*) '     (Pulsed reactor)'
            else
               write(nout,*) '     (Continuous reactor)'
            end if
         end if         

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block PULSE'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block PULSE'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL11(lineno,icode)

C  Handle the NAMELIST input data for block 'FWBLSH'.
C
C  Reads data in NAMELIST format
C
C  $FWBLSH
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/fwblsh/
c  +     denstl, emult, fblbe, fblli2o, fbllipb, fblli, fblss, fblvd,
c  +     fhole, fvolcry, fvolbi, fvolbo, fvolsi, fvolso, vfblkt,
c  +     vfshld, fwclfr, fvoldw, xtfi, xtfo, xtb, xpf, xdo, xdi, estr,
c  +     astr, bstr, costr, fkblkt, smstr, ph, pr, pin, pc, etahp,
c  +     etainp, etalp, etafp, etacp, nipfwh, nlpfwh, sgeff, lblnkt
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'blanket.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'DENSTL') then
         if (isub1.ne.9999) goto 910
         oldval = denstl
         call getrv(nin,line,length,iptr,lineno,denstl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DENSTL',denstl,5.0D3,1.0D4)
         if (denstl.ne.oldval) then
            clabel = 'Density of steel (kg/m3),'
            write(nout,*) clabel,varnam(1:8),' = ',denstl
         end if
      else if (varnam(1:varlen).eq.'EMULT') then
         if (isub1.ne.9999) goto 910
         oldval = emult
         call getrv(nin,line,length,iptr,lineno,emult,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'EMULT',emult,1.0D0,2.0D0)
         if (emult.ne.oldval) then
            clabel = 'Energy multip. in blanket and shield,'
            write(nout,*) clabel,varnam(1:8),' = ',emult
         end if
      else if (varnam(1:varlen).eq.'FBLBE') then
         if (isub1.ne.9999) goto 910
         oldval = fblbe
         call getrv(nin,line,length,iptr,lineno,fblbe,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBLBE',fblbe,0.0D0,1.0D0)
         if (fblbe.ne.oldval) then
            clabel = 'Beryllium fraction of blanket,'
            write(nout,*) clabel,varnam(1:8),' = ',fblbe
         end if
      else if (varnam(1:varlen).eq.'FBLLI2O') then
         if (isub1.ne.9999) goto 910
         oldval = fblli2o
         call getrv(nin,line,length,iptr,lineno,fblli2o,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBLLI2O',fblli2o,0.0D0,1.0D0)
         if (fblli2o.ne.oldval) then
            clabel = 'Li2O fraction of blanket,'
            write(nout,*) clabel,varnam(1:8),' = ',fblli2o
         end if
      else if (varnam(1:varlen).eq.'FBLLIPB') then
         if (isub1.ne.9999) goto 910
         oldval = fbllipb
         call getrv(nin,line,length,iptr,lineno,fbllipb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBLLIPB',fbllipb,0.0D0,1.0D0)
         if (fbllipb.ne.oldval) then
            clabel = 'Li-Pb fraction of blanket,'
            write(nout,*) clabel,varnam(1:8),' = ',fbllipb
         end if
      else if (varnam(1:varlen).eq.'FBLLI') then
         if (isub1.ne.9999) goto 910
         oldval = fblli
         call getrv(nin,line,length,iptr,lineno,fblli,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBLLI',fblli,0.0D0,1.0D0)
         if (fblli.ne.oldval) then
            clabel = 'Lithium fraction of blanket,'
            write(nout,*) clabel,varnam(1:8),' = ',fblli
         end if
      else if (varnam(1:varlen).eq.'FBLSS') then
         if (isub1.ne.9999) goto 910
         oldval = fblss
         call getrv(nin,line,length,iptr,lineno,fblss,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBLSS',fblss,0.0D0,1.0D0)
         if (fblss.ne.oldval) then
            clabel = 'Stainless steel fraction of blanket,'
            write(nout,*) clabel,varnam(1:8),' = ',fblss
         end if
      else if (varnam(1:varlen).eq.'FBLVD') then
         if (isub1.ne.9999) goto 910
         oldval = fblvd
         call getrv(nin,line,length,iptr,lineno,fblvd,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBLVD',fblvd,0.0D0,1.0D0)
         if (fblvd.ne.oldval) then
            clabel = 'Vanadium fraction of blanket,'
            write(nout,*) clabel,varnam(1:8),' = ',fblvd
         end if
      else if (varnam(1:varlen).eq.'FHOLE') then
         if (isub1.ne.9999) goto 910
         oldval = fhole
         call getrv(nin,line,length,iptr,lineno,fhole,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FHOLE',fhole,0.0D0,1.0D0)
         if (fhole.ne.oldval) then
            clabel = 'Hole frac of 1st wall (to neutrons),'
            write(nout,*) clabel,varnam(1:8),' = ',fhole
         end if
      else if (varnam(1:varlen).eq.'FVOLCRY') then
         if (isub1.ne.9999) goto 910
         oldval = fvolcry
         call getrv(nin,line,length,iptr,lineno,fvolcry,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FVOLCRY',fvolcry,0.0D0,10.0D0)
         if (fvolcry.ne.oldval) then
            clabel = 'Fudge factor for cryostat volume,'
            write(nout,*) clabel,varnam(1:8),' = ',fvolcry
         end if
      else if (varnam(1:varlen).eq.'FVOLBI') then
         if (isub1.ne.9999) goto 910
         oldval = fvolbi
         call getrv(nin,line,length,iptr,lineno,fvolbi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FVOLBI',fvolbi,0.0D0,10.0D0)
         if (fvolbi.ne.oldval) then
            clabel = 'Fudge factor for inner blanket volume,'
            write(nout,*) clabel,varnam(1:8),' = ',fvolbi
         end if
      else if (varnam(1:varlen).eq.'FVOLBO') then
         if (isub1.ne.9999) goto 910
         oldval = fvolbo
         call getrv(nin,line,length,iptr,lineno,fvolbo,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FVOLBO',fvolbo,0.0D0,10.0D0)
         if (fvolbo.ne.oldval) then
            clabel = 'Fudge factor for outer blanket volume,'
            write(nout,*) clabel,varnam(1:8),' = ',fvolbo
         end if
      else if (varnam(1:varlen).eq.'FVOLSI') then
         if (isub1.ne.9999) goto 910
         oldval = fvolsi
         call getrv(nin,line,length,iptr,lineno,fvolsi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FVOLSI',fvolsi,0.0D0,10.0D0)
         if (fvolsi.ne.oldval) then
            clabel = 'Fudge factor for inner shield volume,'
            write(nout,*) clabel,varnam(1:8),' = ',fvolsi
         end if
      else if (varnam(1:varlen).eq.'FVOLSO') then
         if (isub1.ne.9999) goto 910
         oldval = fvolso
         call getrv(nin,line,length,iptr,lineno,fvolso,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FVOLSO',fvolso,0.0D0,10.0D0)
         if (fvolso.ne.oldval) then
            clabel = 'Fudge factor for outer shield volume,'
            write(nout,*) clabel,varnam(1:8),' = ',fvolso
         end if
      else if (varnam(1:varlen).eq.'VFBLKT') then
         if (isub1.ne.9999) goto 910
         oldval = vfblkt
         call getrv(nin,line,length,iptr,lineno,vfblkt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'VFBLKT',vfblkt,0.0D0,1.0D0)
         if (vfblkt.ne.oldval) then
            clabel = 'Coolant void fraction in blanket,'
            write(nout,*) clabel,varnam(1:8),' = ',vfblkt
         end if
      else if (varnam(1:varlen).eq.'VFSHLD') then
         if (isub1.ne.9999) goto 910
         oldval = vfshld
         call getrv(nin,line,length,iptr,lineno,vfshld,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'VFSHLD',vfshld,0.0D0,1.0D0)
         if (vfshld.ne.oldval) then
            clabel = 'Coolant void fraction in shield,'
            write(nout,*) clabel,varnam(1:8),' = ',vfshld
         end if

C+**PJK Removed redundant variables DFWI and DFWO

      else if (varnam(1:varlen).eq.'FWCLFR') then
         if (isub1.ne.9999) goto 910
         oldval = fwclfr
         call getrv(nin,line,length,iptr,lineno,fwclfr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FWCLFR',fwclfr,0.0D0,1.0D0)
         if (fwclfr.ne.oldval) then
            clabel = 'First wall coolant fraction,'
            write(nout,*) clabel,varnam(1:8),' = ',fwclfr
         end if
      else if (varnam(1:varlen).eq.'FVOLDW') then
         if (isub1.ne.9999) goto 910
         oldval = fvoldw
         call getrv(nin,line,length,iptr,lineno,fvoldw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FVOLDW',fvoldw,0.0D0,10.0D0)
         if (fvoldw.ne.oldval) then
            clabel = 'Fudge factor for dewar,'
            write(nout,*) clabel,varnam(1:8),' = ',fvoldw
         end if

c+**CAG 19/05/93 Added new input variables necessary for the
c+**CAG 19/05/93 new blanket model.

      else if (varnam(1:varlen).eq.'LBLNKT') then
         if (isub1.ne.9999) goto 910
         ioldvl = LBLNKT
         call getiv(nin,line,length,iptr,lineno,lblnkt,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'LBLNKT',lblnkt,0,1)
         if (lblnkt.ne.ioldvl) then
            clabel = 'Switch for blanket model invoked,'
            write(nout,*) clabel,varnam(1:8),' = ',lblnkt
            if (lblnkt.eq.1) then
               write(nout,*) '     (New model)'
            else
               write(nout,*) '     (Old model)'
            end if
         end if         
      else if (varnam(1:varlen).eq.'COSTR') then
         if (isub1.ne.9999) goto 910
         ioldvl = costr
         call getiv(nin,line,length,iptr,lineno,costr,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'COSTR',costr,1,2)
         if (costr.ne.ioldvl) then
            clabel = 'Switch for blanket coolant material,'
            write(nout,*) clabel,varnam(1:8),' = ',costr
            if (costr.eq.1) then
               write(nout,*) '     (Gaseous helium coolant)'
            else
               write(nout,*) '     (Pressurized water coolant)'
            end if
         end if
      else if (varnam(1:varlen).eq.'XTFI') then
         if (isub1.ne.9999) goto 910
         oldval = xtfi
         call getrv(nin,line,length,iptr,lineno,xtfi,icode)
         if (icode.ne.0) goto 900
         if(costr.eq.1) then
            call ranger(nout,'XTFI',xtfi,250.0D0,350.0D0)
         else   
            call ranger(nout,'XTFI',xtfi,200.0D0,300.0D0)
         endif   
         if (xtfi.ne.oldval) then
            clabel = 'Blanket coolant inlet temperature (C),'
            write(nout,*) clabel,varnam(1:8),' = ',xtfi
         end if
      else if (varnam(1:varlen).eq.'XTFO') then
         if (isub1.ne.9999) goto 910
         oldval = xtfo
         call getrv(nin,line,length,iptr,lineno,xtfo,icode)
         if (icode.ne.0) goto 900
         if(costr.eq.1) then
            call ranger(nout,'XTFO',xtfo,500.0D0,800.0D0)
         else   
            call ranger(nout,'XTFO',xtfo,295.0D0,342.0D0)
         endif   
         if (xtfo.ne.oldval) then
            clabel = 'Blanket coolant outlet temperature (C),'
            write(nout,*) clabel,varnam(1:8),' = ',xtfo
         end if
      else if (varnam(1:varlen).eq.'XTB') then
         if (isub1.ne.9999) goto 910
         oldval = xtb
         call getrv(nin,line,length,iptr,lineno,xtb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'XTB',xtb,400.0D0,800.0D0)
         if (xtb.ne.oldval) then
            clabel = 'Maximum blanket temperature (C),'
            write(nout,*) clabel,varnam(1:8),' = ',xtb
         end if
      else if (varnam(1:varlen).eq.'XPF') then
         if (isub1.ne.9999) goto 910
         oldval = xpf
         call getrv(nin,line,length,iptr,lineno,xpf,icode)
         if (icode.ne.0) goto 900
         if(costr.eq.1) then
            call ranger(nout,'XPF',xpf,1.0D0,5.0D0)
         else   
            call ranger(nout,'XPF',xpf,8.0D0,15.0D0)
         endif   
         if (xpf.ne.oldval) then
            clabel = 'Blanket coolant inlet pressure (MPa),'
            write(nout,*) clabel,varnam(1:8),' = ',xpf
         end if
      else if (varnam(1:varlen).eq.'XDO') then
         if (isub1.ne.9999) goto 910
         oldval = xdo
         call getrv(nin,line,length,iptr,lineno,xdo,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'XDO',xdo,1.0D0,10.0D0)
         if (xdo.ne.oldval) then
            clabel = 'Outer cooling channel diameter (cm),'
            write(nout,*) clabel,varnam(1:8),' = ',xdo
         end if
      else if (varnam(1:varlen).eq.'XDI') then
         if (isub1.ne.9999) goto 910
         oldval = xdi
         call getrv(nin,line,length,iptr,lineno,xdi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'XDI',xdi,1.0D0,10.0D0)
         if (xdi.ne.oldval) then
            clabel = 'Inner cooling channel diameter (cm),'
            write(nout,*) clabel,varnam(1:8),' = ',xdi
         end if
      else if (varnam(1:varlen).eq.'ESTR') then
         if (isub1.ne.9999) goto 910
         ioldvl = estr
         call getiv(nin,line,length,iptr,lineno,estr,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ESTR',estr,1,2)
         if (estr.ne.ioldvl) then
            clabel = 'Switch for cooling channel orientation,'
            write(nout,*) clabel,varnam(1:8),' = ',estr
            if (estr.eq.1) then
               write(nout,*) '     (Radially orientated)'
            else
               write(nout,*) '     (Poloidally orientated)'
            end if
         end if
      else if (varnam(1:varlen).eq.'ASTR') then
         if (isub1.ne.9999) goto 910
         ioldvl = astr
         call getiv(nin,line,length,iptr,lineno,astr,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ASTR',astr,1,2)
         if (astr.ne.ioldvl) then
            clabel = 'Switch for cooling channel geometry,'
            write(nout,*) clabel,varnam(1:8),' = ',astr
            if (astr.eq.1) then
               write(nout,*) '     (Circular cooling channel)'
            else
               write(nout,*) '     (Annular cooling channel)'
            end if
         end if
      else if (varnam(1:varlen).eq.'BSTR') then
         if (isub1.ne.9999) goto 910
         ioldvl = bstr
         call getiv(nin,line,length,iptr,lineno,bstr,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'BSTR',bstr,1,2)
         if (bstr.ne.ioldvl) then
            clabel = 'Switch for blanket boundary condition,'
            write(nout,*) clabel,varnam(1:8),' = ',bstr
            if (bstr.eq.1) then
               write(nout,*) '     (Fixed coolant outlet temperature)'
            else
               write(nout,*) '     (Fixed maximum blanket temperature)'
            end if
         end if
      else if (varnam(1:varlen).eq.'FKBLKT') then
         if (isub1.ne.9999) goto 910
         oldval = fkblkt
         call getrv(nin,line,length,iptr,lineno,fkblkt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FKBLKT',fkblkt,0.2D0,5.0D0)
         if (fkblkt.ne.oldval) then
            clabel = 'Blanket elongation / plasma elongation,'
            write(nout,*) clabel,varnam(1:8),' = ',fkblkt
         end if
      else if (varnam(1:varlen).eq.'SMSTR') then
         if (isub1.ne.9999) goto 910
         ioldvl = smstr
         call getiv(nin,line,length,iptr,lineno,smstr,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SMSTR',smstr,1,2)
         if (smstr.ne.ioldvl) then
            clabel = 'Switch for blanket material,'
            write(nout,*) clabel,varnam(1:8),' = ',smstr
            if (smstr.eq.1) then
               write(nout,*) '     (Li2O/Be blanket)'
            else
               write(nout,*) '     (LiPb/Li blanket)'
            end if
         end if
      else if (varnam(1:varlen).eq.'PH') then
         if (isub1.ne.9999) goto 910
         oldval = ph
         call getrv(nin,line,length,iptr,lineno,ph,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PH',ph,5.0D0,20.0D0)
         if (ph.ne.oldval) then
            clabel = 'High pressure inlet pressure (MPa),'
            write(nout,*) clabel,varnam(1:8),' = ',ph
         end if
      else if (varnam(1:varlen).eq.'PR') then
         if (isub1.ne.9999) goto 910
         oldval = pr
         call getrv(nin,line,length,iptr,lineno,pr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PR',pr,1.0D0,5.0D0)
         if (pr.ne.oldval) then
            clabel = 'Reheat intermediate pressure (MPa),'
            write(nout,*) clabel,varnam(1:8),' = ',pr
         end if
      else if (varnam(1:varlen).eq.'PIN') then
         if (isub1.ne.9999) goto 910
         oldval = pin
         call getrv(nin,line,length,iptr,lineno,pin,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PIN',pin,0.2D0,1.0D0)
         if (pin.ne.oldval) then
            clabel = 'Low pressure inlet pressure (MPa),'
            write(nout,*) clabel,varnam(1:8),' = ',pin
         end if
      else if (varnam(1:varlen).eq.'PC') then
         if (isub1.ne.9999) goto 910
         oldval = pc
         call getrv(nin,line,length,iptr,lineno,pc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PC',pc,0.004D0,0.01D0)
         if (pc.ne.oldval) then
            clabel = 'Codenser pressure (MPa),'
            write(nout,*) clabel,varnam(1:8),' = ',pc
         end if
      else if (varnam(1:varlen).eq.'ETAHP') then
         if (isub1.ne.9999) goto 910
         oldval = etahp
         call getrv(nin,line,length,iptr,lineno,etahp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAHP',etahp,0.1D0,1.0D0)
         if (etahp.ne.oldval) then
            clabel = 'HP turbine isentropic efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etahp
         end if
      else if (varnam(1:varlen).eq.'ETAINP') then
         if (isub1.ne.9999) goto 910
         oldval = etainp
         call getrv(nin,line,length,iptr,lineno,etainp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAINP',etainp,0.1D0,1.0D0)
         if (etainp.ne.oldval) then
            clabel = 'IP turbine isentropic efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etainp
         end if
      else if (varnam(1:varlen).eq.'ETALP') then
         if (isub1.ne.9999) goto 910
         oldval = etalp
         call getrv(nin,line,length,iptr,lineno,etalp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETALP',etalp,0.1D0,1.0D0)
         if (etalp.ne.oldval) then
            clabel = 'LP turbine isentropic efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etalp
         end if
      else if (varnam(1:varlen).eq.'ETAFP') then
         if (isub1.ne.9999) goto 910
         oldval = etafp
         call getrv(nin,line,length,iptr,lineno,etafp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAFP',etafp,0.1D0,1.0D0)
         if (etafp.ne.oldval) then
            clabel = 'Feed pump isentropic efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etafp
         end if
      else if (varnam(1:varlen).eq.'ETACP') then
         if (isub1.ne.9999) goto 910
         oldval = etacp
         call getrv(nin,line,length,iptr,lineno,etacp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETACP',etacp,0.1D0,1.0D0)
         if (etacp.ne.oldval) then
            clabel = 'Condenser isentropic efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etacp
         end if
      else if (varnam(1:varlen).eq.'NIPFWH') then
         if (isub1.ne.9999) goto 910
         ioldvl = nipfwh
         call getiv(nin,line,length,iptr,lineno,nipfwh,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'NIPFWH',nipfwh,1,4)
         if (nipfwh.ne.ioldvl) then
            clabel = 'Number of IP feed water heater pumps,'
            write(nout,*) clabel,varnam(1:8),' = ',nipfwh
         end if
      else if (varnam(1:varlen).eq.'NLPFWH') then
         if (isub1.ne.9999) goto 910
         ioldvl = nlpfwh
         call getiv(nin,line,length,iptr,lineno,nlpfwh,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'NLPFWH',nlpfwh,1,4)
         if (nlpfwh.ne.ioldvl) then
            clabel = 'Number of LP feed water heater pumps,'
            write(nout,*) clabel,varnam(1:8),' = ',nlpfwh
         end if
      else if (varnam(1:varlen).eq.'SGEFF') then
         if (isub1.ne.9999) goto 910
         oldval = sgeff
         call getrv(nin,line,length,iptr,lineno,sgeff,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SGEFF',sgeff,0.1D0,1.0D0)
         if (sgeff.ne.oldval) then
            clabel = 'Steam generator effectiveness,'
            write(nout,*) clabel,varnam(1:8),' = ',sgeff
         end if
      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block FWBLSH'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block FWBLSH'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL12(lineno,icode)

C  Handle the NAMELIST input data for block 'HTPWR'.
C
C  Reads data in NAMELIST format
C
C  $HTPWR
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/htpwr/
c  +     etath, fauxbop, ffwlg, htpmw, trithtmw, vachtmw
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'htpwr.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ETATH') then
         if (isub1.ne.9999) goto 910
         oldval = etath
         call getrv(nin,line,length,iptr,lineno,etath,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETATH',etath,0.0D0,1.0D0)
         if (etath.ne.oldval) then
            clabel = 'Thermal-electric conversion efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',etath
         end if
      else if (varnam(1:varlen).eq.'FAUXBOP') then
         if (isub1.ne.9999) goto 910
         oldval = fauxbop
         call getrv(nin,line,length,iptr,lineno,fauxbop,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FAUXBOP',fauxbop,0.0D0,1.0D0)
         if (fauxbop.ne.oldval) then
            clabel = 'Frac. of gross electric power to BOP,'
            write(nout,*) clabel,varnam(1:8),' = ',fauxbop
         end if
      else if (varnam(1:varlen).eq.'FFWLG') then
         if (isub1.ne.9999) goto 910
         oldval = ffwlg
         call getrv(nin,line,length,iptr,lineno,ffwlg,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FFWLG',ffwlg,0.0D0,1.0D0)
         if (ffwlg.ne.oldval) then
            clabel = '1st wall/dvrtr power frac to lg heat,'
            write(nout,*) clabel,varnam(1:8),' = ',ffwlg
         end if
C+**PJK Added htpmw
      else if (varnam(1:varlen).eq.'HTPMW') then
         if (isub1.ne.9999) goto 910
         oldval = htpmw
         call getrv(nin,line,length,iptr,lineno,htpmw,icode)
         if (icode.ne.0) goto 900
C+**PJK 15/06/04 Upper limit of HTPMW raised to 500 MW
         call ranger(nout,'HTPMW',htpmw,0.0D0,500.0D0)
         if (htpmw.ne.oldval) then
            clabel = 'Heat transport system pump power,'
            write(nout,*) clabel,varnam(1:8),' = ',htpmw
         end if
C+**PJK 15/06/04 Added IPRIMHTP
      else if (varnam(1:varlen).eq.'IPRIMHTP') then
         if (isub1.ne.9999) goto 910
         ioldvl = iprimhtp
         call getiv(nin,line,length,iptr,lineno,iprimhtp,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IPRIMHTP',iprimhtp,0,1)
         if (iprimhtp.ne.ioldvl) then
            clabel = 'Switch for heat transport pump power,'
            write(nout,*) clabel,varnam(1:8),' = ',iprimhtp
            if (iprimhtp.eq.0) then
               write(nout,*) '     (Secondary heat)'
            else
               write(nout,*) '     (Primary heat)'
            end if
         end if
C+**PJK 17/12/93 Removed (commented out) ihts
c      else if (varnam(1:varlen).eq.'IHTS') then
c         if (isub1.ne.9999) goto 910
c         ioldvl = ihts
c         call getiv(nin,line,length,iptr,lineno,ihts,icode)
c         if (icode.ne.0) goto 900
c         call rangei(nout,'IHTS',ihts,0,1)
c         if (ihts.ne.ioldvl) then
c            clabel = 'Switch for primary coolant,'
c            write(nout,*) clabel,varnam(1:8),' = ',ihts
c            if (ihts.eq.0) then
c               write(nout,*) '     (Water coolant)'
c            else
c               write(nout,*) '     (Liquid metal coolant)'
c            end if
c         end if
      else if (varnam(1:varlen).eq.'TRITHTMW') then
         if (isub1.ne.9999) goto 910
         oldval = trithtmw
         call getrv(nin,line,length,iptr,lineno,trithtmw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TRITHTMW',trithtmw,0.0D0,100.0D0)
         if (trithtmw.ne.oldval) then
            clabel = 'Tritium process power (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',trithtmw
         end if
      else if (varnam(1:varlen).eq.'VACHTMW') then
         if (isub1.ne.9999) goto 910
         oldval = vachtmw
         call getrv(nin,line,length,iptr,lineno,vachtmw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'VACHTMW',vachtmw,0.0D0,100.0D0)
         if (vachtmw.ne.oldval) then
            clabel = 'Vacuum pump power (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',vachtmw
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block HTPWR'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block HTPWR'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL13(lineno,icode)

C  Handle the NAMELIST input data for block 'COSTINP'.
C
C  Reads data in NAMELIST format
C
C  $COSTINP
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/costinp/
c  +     abktflnc, adivflnc, cfactr, cfind, cpstflnc, decomf, dintrt,
c  +     dtlife, fcap0, fcap0cp, fcdfuel, fcr0, fkind, iavail,
c  +     ifueltyp, ireactor, ipnet, lsa, ratecdol, tbktrepl, tcomrepl,
c  +     tdivrepl, tlife, uubop, uucd, uudiv, uufuel, uufw, uumag,
c  +     uuves
c
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'cost.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ABKTFLNC') then
         if (isub1.ne.9999) goto 910
         oldval = abktflnc
         call getrv(nin,line,length,iptr,lineno,abktflnc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ABKTFLNC',abktflnc,0.1D0,100.0D0)
         if (abktflnc.ne.oldval) then
            clabel = 'Allowable blanket fluence (MW-yr/m2),'
            write(nout,*) clabel,varnam(1:8),' = ',abktflnc
         end if
      else if (varnam(1:varlen).eq.'ADIVFLNC') then
         if (isub1.ne.9999) goto 910
         oldval = adivflnc
         call getrv(nin,line,length,iptr,lineno,adivflnc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ADIVFLNC',adivflnc,0.1D0,100.0D0)
         if (adivflnc.ne.oldval) then
            clabel = 'Allowable divertor fluence (MW-yr/m2),'
            write(nout,*) clabel,varnam(1:8),' = ',adivflnc
         end if
      else if (varnam(1:varlen).eq.'CFACTR') then
         if (isub1.ne.9999) goto 910
         oldval = cfactr
         call getrv(nin,line,length,iptr,lineno,cfactr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CFACTR',cfactr,0.0D0,1.0D0)
         if (cfactr.ne.oldval) then
            clabel = 'Plant capacity factor or availability,'
            write(nout,*) clabel,varnam(1:8),' = ',cfactr
         end if
      else if (varnam(1:varlen).eq.'CFIND') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'CFIND',rval,0.1D0,0.5D0)
            cfind(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 110        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'CFIND',rval,0.1D0,0.5D0)
            cfind(isub1) = rval
            isub1 = isub1 + 1
            goto 110
         end if
C+**PJK 30/01/96
      else if (varnam(1:varlen).eq.'CPSTFLNC') then
         if (isub1.ne.9999) goto 910
         oldval = cpstflnc
         call getrv(nin,line,length,iptr,lineno,cpstflnc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CPSTFLNC',cpstflnc,0.01D0,30.0D0)
         if (cpstflnc.ne.oldval) then
            clabel = 'Allowable centrepost neutron fluence (MW-yr/m2),'
            write(nout,*) clabel,varnam(1:8),' = ',cpstflnc
         end if
      else if (varnam(1:varlen).eq.'DECOMF') then
         if (isub1.ne.9999) goto 910
         oldval = decomf
         call getrv(nin,line,length,iptr,lineno,decomf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DECOMF',decomf,0.0D0,1.0D0)
         if (decomf.ne.oldval) then
            clabel = 'Decommissioning fund fraction,'
            write(nout,*) clabel,varnam(1:8),' = ',decomf
         end if
C+**PJK 17/04/96
      else if (varnam(1:varlen).eq.'DINTRT') then
         if (isub1.ne.9999) goto 910
         oldval = dintrt
         call getrv(nin,line,length,iptr,lineno,dintrt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DINTRT',dintrt,0.0D0,0.1D0)
         if (dintrt.ne.oldval) then
            clabel = 'Borrowing - saving interest rate difference,'
            write(nout,*) clabel,varnam(1:8),' = ',dintrt
         end if
C+**PJK 16/04/96
      else if (varnam(1:varlen).eq.'DTLIFE') then
         if (isub1.ne.9999) goto 910
         oldval = dtlife
         call getrv(nin,line,length,iptr,lineno,dtlife,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DTLIFE',dtlife,0.0D0,15.0D0)
         if (dtlife.ne.oldval) then
            clabel = 'Decommissioning time prior to end of plant,'
            write(nout,*) clabel,varnam(1:8),' = ',dtlife
         end if

C+**PJK 19/01/94 Removed obsolete variable EI0

      else if (varnam(1:varlen).eq.'FCAP0') then
         if (isub1.ne.9999) goto 910
         oldval = fcap0
         call getrv(nin,line,length,iptr,lineno,fcap0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCAP0',fcap0,1.0D0,1.5D0)
         if (fcap0.ne.oldval) then
            clabel = 'Ave cost of money for plant construction,'
            write(nout,*) clabel,varnam(1:8),' = ',fcap0
         end if
      else if (varnam(1:varlen).eq.'FCAP0CP') then
         if (isub1.ne.9999) goto 910
         oldval = fcap0cp
         call getrv(nin,line,length,iptr,lineno,fcap0cp,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCAP0CP',fcap0cp,1.0D0,1.5D0)
         if (fcap0cp.ne.oldval) then
            clabel = 'Ave cost of money for replaceable components,'
            write(nout,*) clabel,varnam(1:8),' = ',fcap0cp
         end if
      else if (varnam(1:varlen).eq.'FCDFUEL') then
         if (isub1.ne.9999) goto 910
         oldval = fcdfuel
         call getrv(nin,line,length,iptr,lineno,fcdfuel,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCDFUEL',fcdfuel,0.0D0,1.0D0)
         if (fcdfuel.ne.oldval) then
            clabel = 'Fraction of CD cost assumed fuel cost,'
            write(nout,*) clabel,varnam(1:8),' = ',fcdfuel
         end if
      else if (varnam(1:varlen).eq.'FCR0') then
         if (isub1.ne.9999) goto 910
         oldval = fcr0
         call getrv(nin,line,length,iptr,lineno,fcr0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCR0',fcr0,0.0D0,1.0D0)
         if (fcr0.ne.oldval) then
            clabel = 'Fixed charge rate during construction,'
            write(nout,*) clabel,varnam(1:8),' = ',fcr0
         end if
C+**PJK 07/02/96
      else if (varnam(1:varlen).eq.'FKIND') then
         if (isub1.ne.9999) goto 910
         oldval = fkind
         call getrv(nin,line,length,iptr,lineno,fkind,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FKIND',fkind,0.5D0,1.0D0)
         if (fkind.ne.oldval) then
            clabel = 'Multiplier for Nth of a kind costs,'
            write(nout,*) clabel,varnam(1:8),' = ',fkind
         end if
C+**PJK 19/05/99
      else if (varnam(1:varlen).eq.'IAVAIL') then
         if (isub1.ne.9999) goto 910
         ioldvl = iavail
         call getiv(nin,line,length,iptr,lineno,iavail,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IAVAIL',iavail,0,1)
         if (iavail.ne.ioldvl) then
            clabel = 'Switch for plant availability model,'
            write(nout,*) clabel,varnam(1:8),' = ',iavail
            if (iavail.eq.0) then
               write(nout,*) '     (use input value for CFACTR)'
            else
               write(nout,*) '     (use new availability model)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IFUELTYP') then
         if (isub1.ne.9999) goto 910
         ioldvl = ifueltyp
         call getiv(nin,line,length,iptr,lineno,ifueltyp,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IFUELTYP',ifueltyp,0,1)
         if (ifueltyp.ne.ioldvl) then
            clabel = 'Switch for costing of 1st wall etc.,'
            write(nout,*) clabel,varnam(1:8),' = ',ifueltyp
            if (ifueltyp.eq.0) then
               write(nout,*) '     (treated as capital cost)'
            else
               write(nout,*) '     (treated as part of fuel cost)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IREACTOR') then
         if (isub1.ne.9999) goto 910
         ioldvl = ireactor
         call getiv(nin,line,length,iptr,lineno,ireactor,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IREACTOR',ireactor,0,1)
         if (ireactor.ne.ioldvl) then
            clabel = 'Switch for MWe / C-o-E calculation,'
            write(nout,*) clabel,varnam(1:8),' = ',ireactor
            if (ireactor.eq.0) then
               write(nout,*) '     (No MWe / C-o-E calculation)'
            else
               write(nout,*) '     (MWe / C-o-E are calculated)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IPNET') then
         if (isub1.ne.9999) goto 910
         ioldvl = ipnet
         call getiv(nin,line,length,iptr,lineno,ipnet,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IPNET',ipnet,0,1)
         if (ipnet.ne.ioldvl) then
            clabel = 'Switch for net electric power calc.,'
            write(nout,*) clabel,varnam(1:8),' = ',ipnet
            if (ipnet.eq.0) then
               write(nout,*) '     (Scale to keep net MWe positive)'
            else
               write(nout,*) '     (Allow net MWe to be negative)'
            end if
         end if
      else if (varnam(1:varlen).eq.'LSA') then
         if (isub1.ne.9999) goto 910
         ioldvl = lsa
         call getiv(nin,line,length,iptr,lineno,lsa,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'LSA',lsa,1,4)
         if (lsa.ne.ioldvl) then
            clabel = 'Level of safety assurance,'
            write(nout,*) clabel,varnam(1:8),' = ',lsa
            if (lsa.eq.1) then
               write(nout,*) '     (Plant is truly passively safe)'
            else if (lsa.eq.2) then
               write(nout,*) '     (In between)'
            else if (lsa.eq.3) then
               write(nout,*) '     (In between)'
            else
               write(nout,*) '     (Like a current fission plant)'
            end if
         end if
      else if (varnam(1:varlen).eq.'RATECDOL') then
         if (isub1.ne.9999) goto 910
         oldval = ratecdol
         call getrv(nin,line,length,iptr,lineno,ratecdol,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RATECDOL',ratecdol,0.0D0,0.5D0)
         if (ratecdol.ne.oldval) then
            clabel = 'Effective cost of money,'
            write(nout,*) clabel,varnam(1:8),' = ',ratecdol
         end if

C+**PJK 19/01/94 Removed obsolete variables RATEINT, RATEINTA
C+**PJK 19/01/94 Removed obsolete variable TAXRT
C+**PJK 19/01/94 Removed obsolete variable TIMCONST

C+**PJK 19/05/99
      else if (varnam(1:varlen).eq.'TBKTREPL') then
         if (isub1.ne.9999) goto 910
         oldval = tbktrepl
         call getrv(nin,line,length,iptr,lineno,tbktrepl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TBKTREPL',tbktrepl,0.01D0,2.0D0)
         if (tbktrepl.ne.oldval) then
            clabel = 'Time needed to replace blanket (yr),'
            write(nout,*) clabel,varnam(1:8),' = ',tbktrepl
         end if
      else if (varnam(1:varlen).eq.'TCOMREPL') then
         if (isub1.ne.9999) goto 910
         oldval = tcomrepl
         call getrv(nin,line,length,iptr,lineno,tcomrepl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TCOMREPL',tcomrepl,0.01D0,2.0D0)
         if (tcomrepl.ne.oldval) then
            clabel = 'Time needed to replace blanet+divertor (yr),'
            write(nout,*) clabel,varnam(1:8),' = ',tcomrepl
         end if
      else if (varnam(1:varlen).eq.'TDIVREPL') then
         if (isub1.ne.9999) goto 910
         oldval = tdivrepl
         call getrv(nin,line,length,iptr,lineno,tdivrepl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TDIVREPL',tdivrepl,0.01D0,2.0D0)
         if (tdivrepl.ne.oldval) then
            clabel = 'Time needed to replace divertor (yr),'
            write(nout,*) clabel,varnam(1:8),' = ',tdivrepl
         end if
      else if (varnam(1:varlen).eq.'TLIFE') then
         if (isub1.ne.9999) goto 910
         oldval = tlife
         call getrv(nin,line,length,iptr,lineno,tlife,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TLIFE',tlife,1.0D0,100.0D0)
         if (tlife.ne.oldval) then
            clabel = 'Plant life (yr),'
            write(nout,*) clabel,varnam(1:8),' = ',tlife
         end if
      else if (varnam(1:varlen).eq.'UUBOP') then
         if (isub1.ne.9999) goto 910
         oldval = uubop
         call getrv(nin,line,length,iptr,lineno,uubop,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UUBOP',uubop,0.005D0,0.1D0)
         if (uubop.ne.oldval) then
            clabel = 'Unplanned unavailability for BOP,'
            write(nout,*) clabel,varnam(1:8),' = ',uubop
         end if
      else if (varnam(1:varlen).eq.'UUCD') then
         if (isub1.ne.9999) goto 910
         oldval = uucd
         call getrv(nin,line,length,iptr,lineno,uucd,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UUCD',uucd,0.005D0,0.1D0)
         if (uucd.ne.oldval) then
            clabel = 'Unplanned unavailability for CD system,'
            write(nout,*) clabel,varnam(1:8),' = ',uucd
         end if
      else if (varnam(1:varlen).eq.'UUDIV') then
         if (isub1.ne.9999) goto 910
         oldval = uudiv
         call getrv(nin,line,length,iptr,lineno,uudiv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UUDIV',uudiv,0.005D0,0.1D0)
         if (uudiv.ne.oldval) then
            clabel = 'Unplanned unavailability for divertor,'
            write(nout,*) clabel,varnam(1:8),' = ',uudiv
         end if
      else if (varnam(1:varlen).eq.'UUFUEL') then
         if (isub1.ne.9999) goto 910
         oldval = uufuel
         call getrv(nin,line,length,iptr,lineno,uufuel,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UUFUEL',uufuel,0.005D0,0.1D0)
         if (uufuel.ne.oldval) then
            clabel = 'Unplanned unavailability for fuel system,'
            write(nout,*) clabel,varnam(1:8),' = ',uufuel
         end if
      else if (varnam(1:varlen).eq.'UUFW') then
         if (isub1.ne.9999) goto 910
         oldval = uufw
         call getrv(nin,line,length,iptr,lineno,uufw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UUFW',uufw,0.005D0,0.1D0)
         if (uufw.ne.oldval) then
            clabel = 'Unplanned unavailability for first wall,'
            write(nout,*) clabel,varnam(1:8),' = ',uufw
         end if
      else if (varnam(1:varlen).eq.'UUMAG') then
         if (isub1.ne.9999) goto 910
         oldval = uumag
         call getrv(nin,line,length,iptr,lineno,uumag,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UUMAG',uumag,0.005D0,0.1D0)
         if (uumag.ne.oldval) then
            clabel = 'Unplanned unavailability for magnets,'
            write(nout,*) clabel,varnam(1:8),' = ',uumag
         end if
      else if (varnam(1:varlen).eq.'UUVES') then
         if (isub1.ne.9999) goto 910
         oldval = uuves
         call getrv(nin,line,length,iptr,lineno,uuves,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UUVES',uuves,0.005D0,0.1D0)
         if (uuves.ne.oldval) then
            clabel = 'Unplanned unavailability for vessel,'
            write(nout,*) clabel,varnam(1:8),' = ',uuves
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block COSTINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block COSTINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL14(lineno,icode)

C  Handle the NAMELIST input data for block 'UCSTINP'.
C
C  Reads data in NAMELIST format
C
C  $UCSTINP
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/ucstinp/
c  +     cconfix, cconshpf, cconshtf, cland, cowner, csi, cturbb,
c  +     fcontng, ucblbe, ucblli, ucblli2o, ucbllipb, ucblss,
c  +     ucblvd, ucbus, uccase, uccpcl1, uccpclb, uccry, uccryo,
c  +     uccu, ucdiv, ucech, ucf1, ucfnc, ucfuel, uche3, uchrs, uchts,
c  +     uciac, ucich, ucihx, uclh, ucme, ucmisc, ucnbi, ucoam, ucof,
c  +     ucpens, ucpfb, ucpfbk, ucpfbs, ucpfcb, ucpfdr1, ucpfic,
c  +     ucpfps, ucrb, ucsc, ucshld, uctfbr, uctfbus, uctfps, uctfsw,
c  +     ucturb, ucwindpf, ucwindtf, ucwst
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'cost.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'CCONFIX') then
         if (isub1.ne.9999) goto 910
         oldval = cconfix
         call getrv(nin,line,length,iptr,lineno,cconfix,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CCONFIX',cconfix,50.0D0,200.0D0)
         if (cconfix.ne.oldval) then
            clabel = 'Fixed cost of superconducting cable ($/m),'
            write(nout,*) clabel,varnam(1:8),' = ',cconfix
         end if
      else if (varnam(1:varlen).eq.'CCONSHPF') then
         if (isub1.ne.9999) goto 910
         oldval = cconshpf
         call getrv(nin,line,length,iptr,lineno,cconshpf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CCONSHPF',cconshpf,50.0D0,200.0D0)
         if (cconshpf.ne.oldval) then
            clabel = 'PF coil steel conduit/sheath cost ($/m),'
            write(nout,*) clabel,varnam(1:8),' = ',cconshpf
         end if
      else if (varnam(1:varlen).eq.'CCONSHTF') then
         if (isub1.ne.9999) goto 910
         oldval = cconshtf
         call getrv(nin,line,length,iptr,lineno,cconshtf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CCONSHTF',cconshtf,50.0D0,200.0D0)
         if (cconshtf.ne.oldval) then
            clabel = 'TF coil steel conduit/sheath cost ($/m),'
            write(nout,*) clabel,varnam(1:8),' = ',cconshtf
         end if
      else if (varnam(1:varlen).eq.'CLAND') then
         if (isub1.ne.9999) goto 910
         oldval = cland
         call getrv(nin,line,length,iptr,lineno,cland,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CLAND',cland,10.0D0,100.0D0)
         if (cland.ne.oldval) then
            clabel = 'Cost of land (M$),'
            write(nout,*) clabel,varnam(1:8),' = ',cland
         end if
      else if (varnam(1:varlen).eq.'COWNER') then
         if (isub1.ne.9999) goto 910
         oldval = cowner
         call getrv(nin,line,length,iptr,lineno,cowner,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'COWNER',cowner,0.0D0,1.0D0)
         if (cowner.ne.oldval) then
            clabel = 'Owner cost factor,'
            write(nout,*) clabel,varnam(1:8),' = ',cowner
         end if
      else if (varnam(1:varlen).eq.'CSI') then
         if (isub1.ne.9999) goto 910
         oldval = csi
         call getrv(nin,line,length,iptr,lineno,csi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CSI',csi,1.0D0,100.0D0)
         if (csi.ne.oldval) then
            clabel = 'Allowance for site costs (M$),'
            write(nout,*) clabel,varnam(1:8),' = ',csi
         end if
      else if (varnam(1:varlen).eq.'CTURBB') then
         if (isub1.ne.9999) goto 910
         oldval = cturbb
         call getrv(nin,line,length,iptr,lineno,cturbb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CTURBB',cturbb,100.0D0,1000.0D0)
         if (cturbb.ne.oldval) then
            clabel = 'Cost of turbine building (M$),'
            write(nout,*) clabel,varnam(1:8),' = ',cturbb
         end if

C+**PJK 22/12/93 Removed redundant variable EXPEL
C+**PJK 22/12/93 Removed redundant variable EXPHTS
C+**PJK 22/12/93 Removed redundant variable EXPRF

      else if (varnam(1:varlen).eq.'FCONTNG') then
         if (isub1.ne.9999) goto 910
         oldval = fcontng
         call getrv(nin,line,length,iptr,lineno,fcontng,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FCONTNG',fcontng,0.0D0,1.0D0)
         if (fcontng.ne.oldval) then
            clabel = 'Project contingency factor,'
            write(nout,*) clabel,varnam(1:8),' = ',fcontng
         end if
      else if (varnam(1:varlen).eq.'UCBLBE') then
         if (isub1.ne.9999) goto 910
         oldval = ucblbe
         call getrv(nin,line,length,iptr,lineno,ucblbe,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCBLBE',ucblbe,1.0D0,1.0D3)
         if (ucblbe.ne.oldval) then
            clabel = 'Unit cost for blanket Be ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucblbe
         end if

C+**PJK 11/11/92 Removed redundant variable UCBLD

      else if (varnam(1:varlen).eq.'UCBLLI') then
         if (isub1.ne.9999) goto 910
         oldval = ucblli
         call getrv(nin,line,length,iptr,lineno,ucblli,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCBLLI',ucblli,1.0D2,1.0D4)
         if (ucblli.ne.oldval) then
            clabel = 'Unit cost for blanket Li ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucblli
         end if
      else if (varnam(1:varlen).eq.'UCBLLI2O') then
         if (isub1.ne.9999) goto 910
         oldval = ucblli2o
         call getrv(nin,line,length,iptr,lineno,ucblli2o,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCBLLI2O',ucblli2o,1.0D2,1.0D4)
         if (ucblli2o.ne.oldval) then
            clabel = 'Unit cost for blanket Li2O ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucblli2o
         end if
      else if (varnam(1:varlen).eq.'UCBLLIPB') then
         if (isub1.ne.9999) goto 910
         oldval = ucbllipb
         call getrv(nin,line,length,iptr,lineno,ucbllipb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCBLLIPB',ucbllipb,1.0D2,1.0D4)
         if (ucbllipb.ne.oldval) then
            clabel = 'Unit cost for blanket Li-Pb ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucbllipb
         end if
      else if (varnam(1:varlen).eq.'UCBLSS') then
         if (isub1.ne.9999) goto 910
         oldval = ucblss
         call getrv(nin,line,length,iptr,lineno,ucblss,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCBLSS',ucblss,1.0D1,1.0D3)
         if (ucblss.ne.oldval) then
            clabel = 'Unit cost for blanket st.steel ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucblss
         end if
      else if (varnam(1:varlen).eq.'UCBLVD') then
         if (isub1.ne.9999) goto 910
         oldval = ucblvd
         call getrv(nin,line,length,iptr,lineno,ucblvd,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCBLVD',ucblvd,1.0D2,1.0D3)
         if (ucblvd.ne.oldval) then
            clabel = 'Unit cost for blanket Vd ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucblvd
         end if
      else if (varnam(1:varlen).eq.'UCBUS') then
         if (isub1.ne.9999) goto 910
         oldval = ucbus
         call getrv(nin,line,length,iptr,lineno,ucbus,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCBUS',ucbus,0.01D0,10.0D0)
         if (ucbus.ne.oldval) then
            clabel = 'Cost of Al bus for TF coil ($/A-m),'
            write(nout,*) clabel,varnam(1:8),' = ',ucbus
         end if
      else if (varnam(1:varlen).eq.'UCCASE') then
         if (isub1.ne.9999) goto 910
         oldval = uccase
         call getrv(nin,line,length,iptr,lineno,uccase,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCCASE',uccase,1.0D0,1.0D3)
         if (uccase.ne.oldval) then
            clabel = 'Cost of superconductor case ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',uccase
         end if

C+**PJK 13/01/94 Removed redundant variable UCCOND

      else if (varnam(1:varlen).eq.'UCCPCL1') then
         if (isub1.ne.9999) goto 910
         oldval = uccpcl1
         call getrv(nin,line,length,iptr,lineno,uccpcl1,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCCPCL1',uccpcl1,1.0D0,1.0D3)
         if (uccpcl1.ne.oldval) then
            clabel = 'Cost of tapered copper ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',uccpcl1
         end if
      else if (varnam(1:varlen).eq.'UCCPCLB') then
         if (isub1.ne.9999) goto 910
         oldval = uccpclb
         call getrv(nin,line,length,iptr,lineno,uccpclb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCCPCLB',uccpclb,1.0D0,1.0D3)
         if (uccpclb.ne.oldval) then
            clabel = 'Cost TF outer leg plate coils ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',uccpclb
         end if
      else if (varnam(1:varlen).eq.'UCCRY') then
         if (isub1.ne.9999) goto 910
         oldval = uccry
         call getrv(nin,line,length,iptr,lineno,uccry,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCCRY',uccry,1.0D4,1.0D6)
         if (uccry.ne.oldval) then
            clabel = 'Heat transport cryoplant costs ($/W),'
            write(nout,*) clabel,varnam(1:8),' = ',uccry
         end if
      else if (varnam(1:varlen).eq.'UCCRYO') then
         if (isub1.ne.9999) goto 910
         oldval = uccryo
         call getrv(nin,line,length,iptr,lineno,uccryo,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCCRYO',uccryo,1.0D0,1.0D3)
         if (uccryo.ne.oldval) then
            clabel = 'Unit cost for cryostat ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',uccryo
         end if
      else if (varnam(1:varlen).eq.'UCCU') then
         if (isub1.ne.9999) goto 910
         oldval = uccu
         call getrv(nin,line,length,iptr,lineno,uccu,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCCU',uccu,1.0D1,1.0D2)
         if (uccu.ne.oldval) then
            clabel = 'Copper in SC cable cost ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',uccu
         end if
      else if (varnam(1:varlen).eq.'UCDIV') then
         if (isub1.ne.9999) goto 910
         oldval = ucdiv
         call getrv(nin,line,length,iptr,lineno,ucdiv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCDIV',ucdiv,1.0D3,1.0D7)
         if (ucdiv.ne.oldval) then
            clabel = 'Cost of divertor blade ($),'
            write(nout,*) clabel,varnam(1:8),' = ',ucdiv
         end if
      else if (varnam(1:varlen).eq.'UCECH') then
         if (isub1.ne.9999) goto 910
         oldval = ucech
         call getrv(nin,line,length,iptr,lineno,ucech,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCECH',ucech,1.0D0,10.0D0)
         if (ucech.ne.oldval) then
            clabel = 'ECH system cost ($/W),'
            write(nout,*) clabel,varnam(1:8),' = ',ucech
         end if

C+**PJK 11/11/92 Removed redundant variable UCEPE
C+**PJK 18/03/97 Added UCF1

      else if (varnam(1:varlen).eq.'UCF1') then
         if (isub1.ne.9999) goto 910
         oldval = ucf1
         call getrv(nin,line,length,iptr,lineno,ucf1,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCF1',ucf1,1.0D6,50.0D6)
         if (ucf1.ne.oldval) then
            clabel = 'Fuelling system cost ($),'
            write(nout,*) clabel,varnam(1:8),' = ',ucf1
         end if
      else if (varnam(1:varlen).eq.'UCFNC') then
         if (isub1.ne.9999) goto 910
         oldval = ucfnc
         call getrv(nin,line,length,iptr,lineno,ucfnc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCFNC',ucfnc,10.0D0,100.0D0)
         if (ucfnc.ne.oldval) then
            clabel = 'Outer PF fence support cost ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucfnc
         end if
      else if (varnam(1:varlen).eq.'UCFUEL') then
         if (isub1.ne.9999) goto 910
         oldval = ucfuel
         call getrv(nin,line,length,iptr,lineno,ucfuel,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCFUEL',ucfuel,1.0D0,10.0D0)
         if (ucfuel.ne.oldval) then
            clabel = 'Cost of fuel (M$/yr),'
            write(nout,*) clabel,varnam(1:8),' = ',ucfuel
         end if
      else if (varnam(1:varlen).eq.'UCHE3') then
         if (isub1.ne.9999) goto 910
         oldval = uche3
         call getrv(nin,line,length,iptr,lineno,uche3,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCHE3',uche3,1.0D5,1.0D7)
         if (uche3.ne.oldval) then
            clabel = 'Cost of He3 fuel ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',uche3
         end if

C+**PJK 11/11/92 Removed redundant variable UCFW

      else if (varnam(1:varlen).eq.'UCHRS') then
         if (isub1.ne.9999) goto 910
         oldval = uchrs
         call getrv(nin,line,length,iptr,lineno,uchrs,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCHRS',uchrs,1.0D7,5.0D8)
         if (uchrs.ne.oldval) then
            clabel = 'Cost of heat rejection system ($),'
            write(nout,*) clabel,varnam(1:8),' = ',uchrs
         end if
      else if (varnam(1:varlen).eq.'UCHTS') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'UCHTS',rval,10.0D0,100.0D0)
            uchts(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 100        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'UCHTS',rval,10.0D0,100.0D0)
            uchts(isub1) = rval
            isub1 = isub1 + 1
            goto 100
         end if
      else if (varnam(1:varlen).eq.'UCIAC') then
         if (isub1.ne.9999) goto 910
         oldval = uciac
         call getrv(nin,line,length,iptr,lineno,uciac,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCIAC',uciac,1.0D7,1.0D9)
         if (uciac.ne.oldval) then
            clabel = 'Cost of instrum, control & diag.($/W),'
            write(nout,*) clabel,varnam(1:8),' = ',uciac
         end if
      else if (varnam(1:varlen).eq.'UCICH') then
         if (isub1.ne.9999) goto 910
         oldval = ucich
         call getrv(nin,line,length,iptr,lineno,ucich,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCICH',ucich,1.0D0,1.0D1)
         if (ucich.ne.oldval) then
            clabel = 'Cost of ICH system ($/W),'
            write(nout,*) clabel,varnam(1:8),' = ',ucich
         end if
      else if (varnam(1:varlen).eq.'UCIHX') then
         if (isub1.ne.9999) goto 910
         oldval = ucihx
         call getrv(nin,line,length,iptr,lineno,ucihx,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCIHX',ucihx,0.0D0,1.0D2)
         if (ucihx.ne.oldval) then
            clabel = 'Cost of intermed. ht exchangers ($/W),'
            write(nout,*) clabel,varnam(1:8),' = ',ucihx
         end if
      else if (varnam(1:varlen).eq.'UCLH') then
         if (isub1.ne.9999) goto 910
         oldval = uclh
         call getrv(nin,line,length,iptr,lineno,uclh,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCLH',uclh,1.0D0,10.0D0)
         if (uclh.ne.oldval) then
            clabel = 'LH system cost ($/W),'
            write(nout,*) clabel,varnam(1:8),' = ',uclh
         end if
      else if (varnam(1:varlen).eq.'UCME') then
         if (isub1.ne.9999) goto 910
         oldval = ucme
         call getrv(nin,line,length,iptr,lineno,ucme,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCME',ucme,1.0D7,1.0D9)
         if (ucme.ne.oldval) then
            clabel = 'Unit cost of maintenance equip. ($/W),'
            write(nout,*) clabel,varnam(1:8),' = ',ucme
         end if
C+**PJK 07/02/96 Changed UCMISC from array to scalar variable
      else if (varnam(1:varlen).eq.'UCMISC') then
         if (isub1.ne.9999) goto 910
         oldval = ucmisc
         call getrv(nin,line,length,iptr,lineno,ucmisc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCMISC',ucmisc,1.0D7,5.0D7)
         if (ucmisc.ne.oldval) then
            clabel = 'Miscellaneous plant allowance ($),'
            write(nout,*) clabel,varnam(1:8),' = ',ucmisc
         end if
      else if (varnam(1:varlen).eq.'UCNBI') then
         if (isub1.ne.9999) goto 910
         oldval = ucnbi
         call getrv(nin,line,length,iptr,lineno,ucnbi,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCNBI',ucnbi,1.0D0,10.0D0)
         if (ucnbi.ne.oldval) then
            clabel = 'NBI system cost ($/W),'
            write(nout,*) clabel,varnam(1:8),' = ',ucnbi
         end if
      else if (varnam(1:varlen).eq.'UCOAM') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'UCOAM',rval,50.0D0,100.0D0)
            ucoam(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 120        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'UCOAM',rval,50.0D0,100.0D0)
            ucoam(isub1) = rval
            isub1 = isub1 + 1
            goto 120
         end if
C+**PJK 06/03/96
      else if (varnam(1:varlen).eq.'UCOF') then
         if (isub1.ne.9999) goto 910
         oldval = ucof
         call getrv(nin,line,length,iptr,lineno,ucof,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCOF',ucof,0.1D0,10.0D0)
         if (ucof.ne.oldval) then
            clabel = 'Oscillating field current drive cost ($/W),'
            write(nout,*) clabel,varnam(1:8),' = ',ucof
         end if
      else if (varnam(1:varlen).eq.'UCPENS') then
         if (isub1.ne.9999) goto 910
         oldval = ucpens
         call getrv(nin,line,length,iptr,lineno,ucpens,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCPENS',ucpens,1.0D0,100.0D0)
         if (ucpens.ne.oldval) then
            clabel = 'Penetration shield cost ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucpens
         end if
      else if (varnam(1:varlen).eq.'UCPFB') then
         if (isub1.ne.9999) goto 910
         oldval = ucpfb
         call getrv(nin,line,length,iptr,lineno,ucpfb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCPFB',ucpfb,1.0D0,1.0D3)
         if (ucpfb.ne.oldval) then
            clabel = 'Cost of PF coil buses ($/kA-m),'
            write(nout,*) clabel,varnam(1:8),' = ',ucpfb
         end if
      else if (varnam(1:varlen).eq.'UCPFBK') then
         if (isub1.ne.9999) goto 910
         oldval = ucpfbk
         call getrv(nin,line,length,iptr,lineno,ucpfbk,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCPFBK',ucpfbk,1.0D3,1.0D5)
         if (ucpfbk.ne.oldval) then
            clabel = 'Cost of PF coil DC breakers ($/MVA),'
            write(nout,*) clabel,varnam(1:8),' = ',ucpfbk
         end if
      else if (varnam(1:varlen).eq.'UCPFBS') then
         if (isub1.ne.9999) goto 910
         oldval = ucpfbs
         call getrv(nin,line,length,iptr,lineno,ucpfbs,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCPFBS',ucpfbs,1.0D3,1.0D4)
         if (ucpfbs.ne.oldval) then
            clabel = 'Cost of PF burn power supplies ($/kW),'
            write(nout,*) clabel,varnam(1:8),' = ',ucpfbs
         end if
      else if (varnam(1:varlen).eq.'UCPFCB') then
         if (isub1.ne.9999) goto 910
         oldval = ucpfcb
         call getrv(nin,line,length,iptr,lineno,ucpfcb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCPFCB',ucpfcb,1.0D3,1.0D5)
         if (ucpfcb.ne.oldval) then
            clabel = 'Cost of PF coil AC breakers ($/circ),'
            write(nout,*) clabel,varnam(1:8),' = ',ucpfcb
         end if
      else if (varnam(1:varlen).eq.'UCPFDR1') then
         if (isub1.ne.9999) goto 910
         oldval = ucpfdr1
         call getrv(nin,line,length,iptr,lineno,ucpfdr1,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCPFDR1',ucpfdr1,1.0D0,1.0D3)
         if (ucpfdr1.ne.oldval) then
            clabel = 'Cost factor for dump resistors ($/MJ),'
            write(nout,*) clabel,varnam(1:8),' = ',ucpfdr1
         end if

C+**PJK 11/11/92 Removed redundant variable UCPFDR2

      else if (varnam(1:varlen).eq.'UCPFIC') then
         if (isub1.ne.9999) goto 910
         oldval = ucpfic
         call getrv(nin,line,length,iptr,lineno,ucpfic,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCPFIC',ucpfic,1.0D3,1.0D5)
         if (ucpfic.ne.oldval) then
            clabel = 'Cost of PF instrum & cont ($/channel),'
            write(nout,*) clabel,varnam(1:8),' = ',ucpfic
         end if
      else if (varnam(1:varlen).eq.'UCPFPS') then
         if (isub1.ne.9999) goto 910
         oldval = ucpfps
         call getrv(nin,line,length,iptr,lineno,ucpfps,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCPFPS',ucpfps,1.0D3,1.0D5)
         if (ucpfps.ne.oldval) then
            clabel = 'Cost of PF coil pulsed P.S. ($/MVA),'
            write(nout,*) clabel,varnam(1:8),' = ',ucpfps
         end if

C+**PJK 11/11/92 Removed redundant variable UCPHXH2O
C+**PJK 11/11/92 Removed redundant variable UCPHXLM

C+**PJK 07/02/96 Changed UCRB from array to scalar variable
      else if (varnam(1:varlen).eq.'UCRB') then
         if (isub1.ne.9999) goto 910
         oldval = ucrb
         call getrv(nin,line,length,iptr,lineno,ucrb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCRB',ucrb,100.0D0,1.0D3)
         if (ucrb.ne.oldval) then
            clabel = 'Cost of reactor building ($/m3),'
            write(nout,*) clabel,varnam(1:8),' = ',ucrb
         end if
      else if (varnam(1:varlen).eq.'UCSC') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'UCSC',rval,1.0D2,2.0D3)
            ucsc(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 140        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'UCSC',rval,1.0D2,2.0D3)
            ucsc(isub1) = rval
            isub1 = isub1 + 1
            goto 140
         end if
      else if (varnam(1:varlen).eq.'UCSHLD') then
         if (isub1.ne.9999) goto 910
         oldval = ucshld
         call getrv(nin,line,length,iptr,lineno,ucshld,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCSHLD',ucshld,1.0D0,100.0D0)
         if (ucshld.ne.oldval) then
            clabel = 'Cost of shield structural steel ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucshld
         end if
C+**PJK 01/03/94 Removed redundant variable UCSHTH
C+**PJK 11/11/92 Removed redundant variable UCSTRUC

      else if (varnam(1:varlen).eq.'UCTFBR') then
         if (isub1.ne.9999) goto 910
         oldval = uctfbr
         call getrv(nin,line,length,iptr,lineno,uctfbr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCTFBR',uctfbr,1.0D0,10.0D0)
         if (uctfbr.ne.oldval) then
            clabel = 'Cost of TF coil breakers ($/W**0.7),'
            write(nout,*) clabel,varnam(1:8),' = ',uctfbr
         end if
      else if (varnam(1:varlen).eq.'UCTFBUS') then
         if (isub1.ne.9999) goto 910
         oldval = uctfbus
         call getrv(nin,line,length,iptr,lineno,uctfbus,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCTFBUS',uctfbus,1.0D0,1.0D3)
         if (uctfbus.ne.oldval) then
            clabel = 'Cost of TF coil bus ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',uctfbus
         end if
      else if (varnam(1:varlen).eq.'UCTFPS') then
         if (isub1.ne.9999) goto 910
         oldval = uctfps
         call getrv(nin,line,length,iptr,lineno,uctfps,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCTFPS',uctfps,1.0D0,1.0D3)
         if (uctfps.ne.oldval) then
            clabel = 'Cost of TF power supplies ($/W**0.7),'
            write(nout,*) clabel,varnam(1:8),' = ',uctfps
         end if
      else if (varnam(1:varlen).eq.'UCTFSW') then
         if (isub1.ne.9999) goto 910
         oldval = uctfsw
         call getrv(nin,line,length,iptr,lineno,uctfsw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCTFSW',uctfsw,0.1D0,10.0D0)
         if (uctfsw.ne.oldval) then
            clabel = 'Cost of TF slow dump switches ($/A),'
            write(nout,*) clabel,varnam(1:8),' = ',uctfsw
         end if
      else if (varnam(1:varlen).eq.'UCTURB') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'UCTURB',rval,1.0D7,1.0D9)
            ucturb(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 150        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'UCTURB',rval,1.0D7,1.0D9)
            ucturb(isub1) = rval
            isub1 = isub1 + 1
            goto 150
         end if

C+**PJK 11/11/92 Removed redundant variable UCVAC

      else if (varnam(1:varlen).eq.'UCWINDPF') then
         if (isub1.ne.9999) goto 910
         oldval = ucwindpf
         call getrv(nin,line,length,iptr,lineno,ucwindpf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCWINDPF',ucwindpf,1.0D2,1.0D3)
         if (ucwindpf.ne.oldval) then
            clabel = 'Cost of SCPF windings ($/m),'
            write(nout,*) clabel,varnam(1:8),' = ',ucwindpf
         end if
      else if (varnam(1:varlen).eq.'UCWINDTF') then
         if (isub1.ne.9999) goto 910
         oldval = ucwindtf
         call getrv(nin,line,length,iptr,lineno,ucwindtf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCWINDTF',ucwindtf,1.0D2,1.0D3)
         if (ucwindtf.ne.oldval) then
            clabel = 'Cost of SCTF windings ($/m),'
            write(nout,*) clabel,varnam(1:8),' = ',ucwindtf
         end if

C+**PJK 01/03/94 Removed redundant variable UCWPW

      else if (varnam(1:varlen).eq.'UCWST') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'UCWST',rval,0.0D0,20.0D0)
            ucwst(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 160        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'UCWST',rval,0.0D0,20.0D0)
            ucwst(isub1) = rval
            isub1 = isub1 + 1
            goto 160
         end if
      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block UCSTINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block UCSTINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL15(lineno,icode)

C  Handle the NAMELIST input data for block 'SWEP'.
C
C  Reads data in NAMELIST format
C
C  $SWEP
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/swep/
c  +     isweep, nsweep, sweep
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'sweep.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ISWEEP') then
         if (isub1.ne.9999) goto 910
         ioldvl = isweep
         call getiv(nin,line,length,iptr,lineno,isweep,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ISWEEP',isweep,0,ipnscns)
         if (isweep.ne.ioldvl) then
            clabel = 'Number of scans to perform,'
            write(nout,*) clabel,varnam(1:8),' = ',isweep
         end if
      else if (varnam(1:varlen).eq.'NSWEEP') then
         if (isub1.ne.9999) goto 910
         ioldvl = nsweep
         call getiv(nin,line,length,iptr,lineno,nsweep,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'NSWEEP',nsweep,1,ipnscnv)
         if (nsweep.ne.ioldvl) then
            clabel = 'Variable used in scan,'
            write(nout,*) clabel,varnam(1:8),' = ',nsweep
            if (nsweep.eq.1) then
               write(nout,*) '     (aspect - aspect ratio)'
            else if (nsweep.eq.2) then
               write(nout,*)
     +              '     (hldivlim - divertor heat load limit)'
            else if (nsweep.eq.3) then
               write(nout,*) '     (pnetelin - net electric power)'
            else if (nsweep.eq.4) then
               write(nout,*)
     +              '     (hfact - energy confinement time h-factor)'
            else if (nsweep.eq.5) then
               write(nout,*)
     +              '     (oacdcp - J in TF coil inner leg)'
            else if (nsweep.eq.6) then
               write(nout,*)
     +              '     (walalw - allowable neutron wall load)'
            else if (nsweep.eq.7) then
               write(nout,*)
     +              '     (beamfus0 - beam-bkg fusion calc multiplier)'
            else if (nsweep.eq.8) then
               write(nout,*)
     +              '     (fqval - f-value for Q)'
            else if (nsweep.eq.9) then
               write(nout,*)
     +              '     (te - electron temperature)'
            else if (nsweep.eq.10) then
               write(nout,*)
     +              '     (boundu(15) - upper bound on fvs)'
            else if (nsweep.eq.11) then
               write(nout,*)
     +              '     (dnbeta - Troyon coefficient)'
            else if (nsweep.eq.12) then
               write(nout,*)
     +              '     (bootstrap current fraction)'
            else if (nsweep.eq.13) then
               write(nout,*)
     +              '     (boundu(10) - upper bound on hfact)'
            else if (nsweep.eq.14) then
               write(nout,*)
     +              '     (fiooic - f-value for TF coil current)'
            else if (nsweep.eq.15) then
               write(nout,*)
     +              '     (fjprot - f-value for TF coil J limit)'
            else if (nsweep.eq.16) then
               write(nout,*)
     +              '     (rmajor - plasma major radius)'
            else if (nsweep.eq.17) then
               write(nout,*)
     +              '     (bmxlim - maximum toroidal field)'
            else if (nsweep.eq.18) then
               write(nout,*)
     +              '     (gammax - maximum C.D. gamma)'
            else if (nsweep.eq.19) then
               write(nout,*)
     +              '     (boundl(16) - lower bound on ohcth)'
            else if (nsweep.eq.20) then
               write(nout,*)
     +              '     (tbrnmn - minimum burn time)'
            else if (nsweep.eq.21) then
               write(nout,*)
     +              '     (sigpfalw - allowable PF coil stress)'
            else if (nsweep.eq.22) then
               write(nout,*)
     +              '     (cfactr - plant availability)'
            else if (nsweep.eq.23) then
               write(nout,*)
     +              '     (boundu(72) - upper bound on fipir)'
            else if (nsweep.eq.24) then
               write(nout,*)
     +              '     (powfmax - fusion power limit)'
            else if (nsweep.eq.25) then
               write(nout,*)
     +              '     (kappa - elongation)'
            else if (nsweep.eq.26) then
               write(nout,*)
     +              '     (triang - triangularity)'
            else
               continue
            end if
         end if
      else if (varnam(1:varlen).eq.'SWEEP') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'SWEEP',rval,-1.0D0,1.0D9)
            sweep(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 100        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'SWEEP',rval,-1.0D0,1.0D9)
            sweep(isub1) = rval
            isub1 = isub1 + 1
            goto 100
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block SWEP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block SWEP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL16(lineno,icode)

C  Handle the NAMELIST input data for block 'BLDINP'.
C
C  Reads data in NAMELIST format
C
C  $BLDINP
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/bldinp/
c  +     esbldgm3,pfbldgm3,tfcbv,triv
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'bldgvol.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

C+**PJK 05/02/96
      if (varnam(1:varlen).eq.'ESBLDGM3') then
         if (isub1.ne.9999) goto 910
         oldval = esbldgm3
         call getrv(nin,line,length,iptr,lineno,esbldgm3,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ESBLDGM3',esbldgm3,1.0D3,1.0D6)
         if (esbldgm3.ne.oldval) then
            clabel = 'Energy storage building volume (m3),'
            write(nout,*) clabel,varnam(1:8),' = ',esbldgm3
         end if
      else if (varnam(1:varlen).eq.'PFBLDGM3') then
         if (isub1.ne.9999) goto 910
         oldval = pfbldgm3
         call getrv(nin,line,length,iptr,lineno,pfbldgm3,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PFBLDGM3',pfbldgm3,1.0D4,1.0D6)
         if (pfbldgm3.ne.oldval) then
            clabel = 'PF coil power conv. bldg volume (m3),'
            write(nout,*) clabel,varnam(1:8),' = ',pfbldgm3
         end if
      else if (varnam(1:varlen).eq.'TFCBV') then
         if (isub1.ne.9999) goto 910
         oldval = tfcbv
         call getrv(nin,line,length,iptr,lineno,tfcbv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TFCBV',tfcbv,1.0D4,1.0D6)
         if (tfcbv.ne.oldval) then
            clabel = 'TF coil power conv. bldg volume (m3),'
            write(nout,*) clabel,varnam(1:8),' = ',tfcbv
         end if
      else if (varnam(1:varlen).eq.'TRIV') then
         if (isub1.ne.9999) goto 910
         oldval = triv
         call getrv(nin,line,length,iptr,lineno,triv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TRIV',triv,1.0D4,1.0D6)
         if (triv.ne.oldval) then
            clabel = 'Tritium building volume (m3),'
            write(nout,*) clabel,varnam(1:8),' = ',triv
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block BLDINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block BLDINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL17(lineno,icode)

C  Handle the NAMELIST input data for block 'HTTINP'.
C
C  Reads data in NAMELIST format
C
C  $HTTINP
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/httinp/
c  +     fmgdmw, tfacpd
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

C+**PJK 22/01/97 heattr.h subsumed into htpwr.h

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'htpwr.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'FMGDMW') then
         if (isub1.ne.9999) goto 910
         oldval = fmgdmw
         call getrv(nin,line,length,iptr,lineno,fmgdmw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FMGDMW',fmgdmw,0.0D0,100.0D0)
         if (fmgdmw.ne.oldval) then
            clabel = 'Power to MGF units (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',fmgdmw
         end if
      else if (varnam(1:varlen).eq.'TFACPD') then
         if (isub1.ne.9999) goto 910
         oldval = tfacpd
         call getrv(nin,line,length,iptr,lineno,tfacpd,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TFACPD',tfacpd,0.0D0,100.0D0)
         if (tfacpd.ne.oldval) then
            clabel = 'Total ss TF coil AC power demand (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',tfacpd
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block HTTINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block HTTINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL18(lineno,icode)

C  Handle the NAMELIST input data for block 'HTRINP'.
C
C  Reads data in NAMELIST format
C
C  $HTRINP
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/htrinp/
c  +     baseel, pwpm2
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

C+**PJK 22/01/97 heatrinp.h subsumed into htpwr.h

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'htpwr.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'BASEEL') then
         if (isub1.ne.9999) goto 910
         oldval = baseel
         call getrv(nin,line,length,iptr,lineno,baseel,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BASEEL',baseel,1.0D6,1.0D10)
         if (baseel.ne.oldval) then
            clabel = 'Base plant electric load (W),'
            write(nout,*) clabel,varnam(1:8),' = ',baseel
         end if
      else if (varnam(1:varlen).eq.'PWPM2') then
         if (isub1.ne.9999) goto 910
         oldval = pwpm2
         call getrv(nin,line,length,iptr,lineno,pwpm2,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PWPM2',pwpm2,0.0D0,1.0D3)
         if (pwpm2.ne.oldval) then
            clabel = 'Base AC power requirement (W/m2),'
            write(nout,*) clabel,varnam(1:8),' = ',pwpm2
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block HTRINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block HTRINP'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL19(lineno,icode)

C  Handle the NAMELIST input data for block 'EST'.
C
C  Reads data in NAMELIST format
C
C  $EST
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/est/
c  +     iscenr
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'estocom.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ISCENR') then
         if (isub1.ne.9999) goto 910
         ioldvl = iscenr
         call getiv(nin,line,length,iptr,lineno,iscenr,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ISCENR',iscenr,1,3)
         if (iscenr.ne.ioldvl) then
            clabel = 'Switch for energy storage option,'
            write(nout,*) clabel,varnam(1:8),' = ',iscenr
            if (iscenr.eq.1) then
               write(nout,*) '     (All power from MGF units)'
            else if (iscenr.eq.2) then
               write(nout,*) '     (All pulsed power from line)'
            else
               write(nout,*)
     +              '     (PF power from MGF, heating from line)'
            end if
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block EST'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block EST'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL20(lineno,icode)

C  Handle the NAMELIST input data for block 'BCOM'.
C
C  Reads data in NAMELIST format
C
C  $BCOM
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/bcom/
c  +     admv, clh1, clh2, conv, fndt, hcwt, hccl, pibv, rbrt, rbwt,
c  +     row, rxcl, shmf, shov, stcl, trcl, wgt, wgt2
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'bldgcom.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ADMV') then
         if (isub1.ne.9999) goto 910
         oldval = admv
         call getrv(nin,line,length,iptr,lineno,admv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ADMV',admv,1.0D4,1.0D6)
         if (admv.ne.oldval) then
            clabel = 'Administration building volume (m3),'
            write(nout,*) clabel,varnam(1:8),' = ',admv
         end if
      else if (varnam(1:varlen).eq.'CLH1') then
         if (isub1.ne.9999) goto 910
         oldval = clh1
         call getrv(nin,line,length,iptr,lineno,clh1,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CLH1',clh1,0.0D0,20.0D0)
         if (clh1.ne.oldval) then
            clabel = 'Clearance TF coil to cryostat top (m),'
            write(nout,*) clabel,varnam(1:8),' = ',clh1
         end if
      else if (varnam(1:varlen).eq.'CLH2') then
         if (isub1.ne.9999) goto 910
         oldval = clh2
         call getrv(nin,line,length,iptr,lineno,clh2,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CLH2',clh2,0.0D0,30.0D0)
         if (clh2.ne.oldval) then
            clabel = 'Clearance TF coil to foundation (m),'
            write(nout,*) clabel,varnam(1:8),' = ',clh2
         end if
      else if (varnam(1:varlen).eq.'CONV') then
         if (isub1.ne.9999) goto 910
         oldval = conv
         call getrv(nin,line,length,iptr,lineno,conv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CONV',conv,1.0D4,1.0D6)
         if (conv.ne.oldval) then
            clabel = 'Control building volume (m3),'
            write(nout,*) clabel,varnam(1:8),' = ',conv
         end if
      else if (varnam(1:varlen).eq.'FNDT') then
         if (isub1.ne.9999) goto 910
         oldval = fndt
         call getrv(nin,line,length,iptr,lineno,fndt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FNDT',fndt,0.0D0,10.0D0)
         if (fndt.ne.oldval) then
            clabel = 'Foundation thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',fndt
         end if
      else if (varnam(1:varlen).eq.'HCWT') then
         if (isub1.ne.9999) goto 910
         oldval = hcwt
         call getrv(nin,line,length,iptr,lineno,hcwt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'HCWT',hcwt,0.0D0,10.0D0)
         if (hcwt.ne.oldval) then
            clabel = 'Hot cell wall thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',hcwt
         end if
      else if (varnam(1:varlen).eq.'HCCL') then
         if (isub1.ne.9999) goto 910
         oldval = hccl
         call getrv(nin,line,length,iptr,lineno,hccl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'HCCL',hccl,0.0D0,10.0D0)
         if (hccl.ne.oldval) then
            clabel = 'Clrnce around cmpnts in hot cell (m),'
            write(nout,*) clabel,varnam(1:8),' = ',hccl
         end if
      else if (varnam(1:varlen).eq.'PIBV') then
         if (isub1.ne.9999) goto 910
         oldval = pibv
         call getrv(nin,line,length,iptr,lineno,pibv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PIBV',pibv,1.0D3,1.0D5)
         if (pibv.ne.oldval) then
            clabel = 'Power injection building volume (m3),'
            write(nout,*) clabel,varnam(1:8),' = ',pibv
         end if
      else if (varnam(1:varlen).eq.'RBRT') then
         if (isub1.ne.9999) goto 910
         oldval = rbrt
         call getrv(nin,line,length,iptr,lineno,rbrt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RBRT',rbrt,0.0D0,10.0D0)
         if (rbrt.ne.oldval) then
            clabel = 'Reactor building roof thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',rbrt
         end if
      else if (varnam(1:varlen).eq.'RBWT') then
         if (isub1.ne.9999) goto 910
         oldval = rbwt
         call getrv(nin,line,length,iptr,lineno,rbwt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RBWT',rbwt,0.0D0,10.0D0)
         if (rbwt.ne.oldval) then
            clabel = 'Reactor building wall thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',rbwt
         end if
      else if (varnam(1:varlen).eq.'ROW') then
         if (isub1.ne.9999) goto 910
         oldval = row
         call getrv(nin,line,length,iptr,lineno,row,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ROW',row,0.0D0,10.0D0)
         if (row.ne.oldval) then
            clabel = 'Wall clearance for cranes (m),'
            write(nout,*) clabel,varnam(1:8),' = ',row
         end if
      else if (varnam(1:varlen).eq.'RXCL') then
         if (isub1.ne.9999) goto 910
         oldval = rxcl
         call getrv(nin,line,length,iptr,lineno,rxcl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RXCL',rxcl,0.0D0,10.0D0)
         if (rxcl.ne.oldval) then
            clabel = 'Clearance around reactor (m),'
            write(nout,*) clabel,varnam(1:8),' = ',rxcl
         end if
      else if (varnam(1:varlen).eq.'SHMF') then
         if (isub1.ne.9999) goto 910
         oldval = shmf
         call getrv(nin,line,length,iptr,lineno,shmf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SHMF',shmf,0.0D0,1.0D0)
         if (shmf.ne.oldval) then
            clabel = 'Fraction of TF shield mass per lift,'
            write(nout,*) clabel,varnam(1:8),' = ',shmf
         end if
      else if (varnam(1:varlen).eq.'SHOV') then
         if (isub1.ne.9999) goto 910
         oldval = shov
         call getrv(nin,line,length,iptr,lineno,shov,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SHOV',shov,1.0D3,1.0D6)
         if (shov.ne.oldval) then
            clabel = 'Shops and warehouse volume (m3),'
            write(nout,*) clabel,varnam(1:8),' = ',shov
         end if
      else if (varnam(1:varlen).eq.'STCL') then
         if (isub1.ne.9999) goto 910
         oldval = stcl
         call getrv(nin,line,length,iptr,lineno,stcl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'STCL',stcl,0.0D0,10.0D0)
         if (stcl.ne.oldval) then
            clabel = 'Clearance above crane to roof (m),'
            write(nout,*) clabel,varnam(1:8),' = ',stcl
         end if
      else if (varnam(1:varlen).eq.'TRCL') then
         if (isub1.ne.9999) goto 910
         oldval = trcl
         call getrv(nin,line,length,iptr,lineno,trcl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TRCL',trcl,0.0D0,10.0D0)
         if (trcl.ne.oldval) then
            clabel = 'Transport clearance between comps (m),'
            write(nout,*) clabel,varnam(1:8),' = ',trcl
         end if
      else if (varnam(1:varlen).eq.'WGT') then
         if (isub1.ne.9999) goto 910
         oldval = wgt
         call getrv(nin,line,length,iptr,lineno,wgt,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'WGT',wgt,1.0D4,1.0D6)
         if (wgt.ne.oldval) then
            clabel = 'Reactor building crane capacity (kg),'
            write(nout,*) clabel,varnam(1:8),' = ',wgt
         end if
      else if (varnam(1:varlen).eq.'WGT2') then
         if (isub1.ne.9999) goto 910
         oldval = wgt2
         call getrv(nin,line,length,iptr,lineno,wgt2,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'WGT2',wgt2,1.0D4,1.0D6)
         if (wgt2.ne.oldval) then
            clabel = 'Hot cell crane capacity (kg),'
            write(nout,*) clabel,varnam(1:8),' = ',wgt2
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block BCOM'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block BCOM'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL21(lineno,icode)

C  Handle the NAMELIST input data for block 'OSECTS'.
C
C  Reads data in NAMELIST format
C
C  $OSECTS
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/osects/
c  +     sect01, sect02, sect03, sect04, sect05, sect06, sect07,
c  +     sect08, sect09, sect10, sect11, sect12, sect13, sect14, 
c  +     sect15, sect16, sect17, sect18, sect19, sect20, sect21
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'osections.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'SECT01') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect01
         call getiv(nin,line,length,iptr,lineno,sect01,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT01',sect01,0,1)
         if (sect01.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect01
            if (sect01.eq.0) then
               write(nout,*)
     +              '     (Power reactor costs output turned off)'
            else
               write(nout,*)
     +              '     (Power reactor costs output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT02') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect02
         call getiv(nin,line,length,iptr,lineno,sect02,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT02',sect02,0,1)
         if (sect02.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect02
            if (sect02.eq.0) then
               write(nout,*)
     +              '     (Detailed costings output turned off)'
            else
               write(nout,*)
     +              '     (Detailed costings output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT03') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect03
         call getiv(nin,line,length,iptr,lineno,sect03,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT03',sect03,0,1)
         if (sect03.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect03
            if (sect03.eq.0) then
               write(nout,*)
     +              '     (Plasma output turned off)'
            else
               write(nout,*)
     +              '     (Plasma output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT04') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect04
         call getiv(nin,line,length,iptr,lineno,sect04,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT04',sect04,0,1)
         if (sect04.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect04
            if (sect04.eq.0) then
               write(nout,*)
     +              '     (Current drive system output turned off)'
            else
               write(nout,*)
     +              '     (Current drive system output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT05') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect05
         call getiv(nin,line,length,iptr,lineno,sect05,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT05',sect05,0,1)
         if (sect05.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect05
            if (sect05.eq.0) then
               write(nout,*)
     +              '     (Divertor output turned off)'
            else
               write(nout,*)
     +              '     (Divertor output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT06') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect06
         call getiv(nin,line,length,iptr,lineno,sect06,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT06',sect06,0,1)
         if (sect06.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect06
            if (sect06.eq.0) then
               write(nout,*)
     +              '     (Machine build output turned off)'
            else
               write(nout,*)
     +              '     (Machine build output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT07') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect07
         call getiv(nin,line,length,iptr,lineno,sect07,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT07',sect07,0,1)
         if (sect07.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect07
            if (sect07.eq.0) then
               write(nout,*)
     +              '     (TF coils output turned off)'
            else
               write(nout,*)
     +              '     (TF coils output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT08') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect08
         call getiv(nin,line,length,iptr,lineno,sect08,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT08',sect08,0,1)
         if (sect08.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect08
            if (sect08.eq.0) then
               write(nout,*)
     +              '     (PF coils output turned off)'
            else
               write(nout,*)
     +              '     (PF coils output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT09') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect09
         call getiv(nin,line,length,iptr,lineno,sect09,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT09',sect09,0,1)
         if (sect09.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect09
            if (sect09.eq.0) then
               write(nout,*)
     +              '     (Volt seconds output turned off)'
            else
               write(nout,*)
     +              '     (Volt seconds output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT10') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect10
         call getiv(nin,line,length,iptr,lineno,sect10,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT10',sect10,0,1)
         if (sect10.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect10
            if (sect10.eq.0) then
               write(nout,*)
     +              '     (Support structure output turned off)'
            else
               write(nout,*)
     +              '     (Support structure output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT11') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect11
         call getiv(nin,line,length,iptr,lineno,sect11,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT11',sect11,0,1)
         if (sect11.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect11
            if (sect11.eq.0) then
               write(nout,*)
     +              '     (PF coil inductances output turned off)'
            else
               write(nout,*)
     +              '     (PF coil inductances output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT12') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect12
         call getiv(nin,line,length,iptr,lineno,sect12,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT12',sect12,0,1)
         if (sect12.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect12
            if (sect12.eq.0) then
               write(nout,*)
     +              '     (Shield / blanket output turned off)'
            else
               write(nout,*)
     +              '     (Shield / blanket output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT13') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect13
         call getiv(nin,line,length,iptr,lineno,sect13,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT13',sect13,0,1)
         if (sect13.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect13
            if (sect13.eq.0) then
               write(nout,*)
     +              '     (Power conversion output turned off)'
            else
               write(nout,*)
     +              '     (Power conversion output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT14') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect14
         call getiv(nin,line,length,iptr,lineno,sect14,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT14',sect14,0,1)
         if (sect14.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect14
            if (sect14.eq.0) then
               write(nout,*)
     +              '     (Heat transport output turned off)'
            else
               write(nout,*)
     +              '     (Heat transport output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT15') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect15
         call getiv(nin,line,length,iptr,lineno,sect15,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT15',sect15,0,1)
         if (sect15.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect15
            if (sect15.eq.0) then
               write(nout,*)
     +              '     (Vacuum system output turned off)'
            else
               write(nout,*)
     +              '     (Vacuum system output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT16') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect16
         call getiv(nin,line,length,iptr,lineno,sect16,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT16',sect16,0,1)
         if (sect16.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect16
            if (sect16.eq.0) then
               write(nout,*)
     +              '     (Plant buildings output turned off)'
            else
               write(nout,*)
     +              '     (Plant buildings output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT17') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect17
         call getiv(nin,line,length,iptr,lineno,sect17,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT17',sect17,0,1)
         if (sect17.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect17
            if (sect17.eq.0) then
               write(nout,*)
     +              '     (AC power output turned off)'
            else
               write(nout,*)
     +              '     (AC power output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT18') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect18
         call getiv(nin,line,length,iptr,lineno,sect18,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT18',sect18,0,1)
         if (sect18.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect18
            if (sect18.eq.0) then
               write(nout,*)
     +              '     (Neutral beams output turned off)'
            else
               write(nout,*)
     +              '     (Neutral beams output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT19') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect19
         call getiv(nin,line,length,iptr,lineno,sect19,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT19',sect19,0,1)
         if (sect19.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect19
            if (sect19.eq.0) then
               write(nout,*)
     +              '     (ECH output turned off)'
            else
               write(nout,*)
     +              '     (ECH output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT20') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect20
         call getiv(nin,line,length,iptr,lineno,sect20,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT20',sect20,0,1)
         if (sect20.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect20
            if (sect20.eq.0) then
               write(nout,*)
     +              '     (Lower hybrid output turned off)'
            else
               write(nout,*)
     +              '     (Lower hybrid output turned on)'
            end if
         end if
      else if (varnam(1:varlen).eq.'SECT21') then
         if (isub1.ne.9999) goto 910
         ioldvl = sect21
         call getiv(nin,line,length,iptr,lineno,sect21,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'SECT21',sect21,0,1)
         if (sect21.ne.ioldvl) then
            clabel = 'Switch for output section,'
            write(nout,*) clabel,varnam(1:8),' = ',sect21
            if (sect21.eq.0) then
               write(nout,*)
     +              '     (Time output turned off)'
            else
               write(nout,*)
     +              '     (Time output turned on)'
            end if
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block OSECTS'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block OSECTS'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C______________________________________________________________________
      SUBROUTINE RDNL22(lineno,icode)

C  Handle the NAMELIST input data for block 'VACCY'.
C
C  Reads data in NAMELIST format
C
C  $VACCY
C  ...
C  $END
C
C  for the NAMELIST block:
C
c   namelist/vaccy/
c  +     prdiv, pbase, tn, rat, ntype
C
C  All data is of the form:
C  variable = value,
C  array(subscript) = value,
C  array = value1, value2, value3 (can be spread over 2 or more lines)

      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'vaccom.h'

      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam

      INTEGER lineno,icode,errlen,length,iptr,varlen,isub1,isub2,
     +     ival,ioldvl,iost

      DOUBLE PRECISION rval,oldval

      INTEGER  lenstr
      EXTERNAL lenstr

      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C  Read next line of NAMELIST data
 10   continue
      read(nin,'(a)',iostat=iost) line

C  On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

c  Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

c  Ignore comments
      if (line(1:1).eq.'*') goto 10

c  If $END, return
      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

c  Length of line in characters (excluding trailing spaces)
      length = lenstr(line,len(line))
      iptr = 1

c  It's not $END, so it must be an assignment statement
c  Get the variable name
      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

c  Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'PRDIV') then
         if (isub1.ne.9999) goto 910
         oldval = prdiv
         call getrv(nin,line,length,iptr,lineno,prdiv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PRDIV',prdiv,0.0D0,10.0D0)
         if (prdiv.ne.oldval) then
            clabel = 'Divertor chamber pressure in burn (Pa),'
            write(nout,*) clabel,varnam(1:8),' = ',prdiv
         end if
      else if (varnam(1:varlen).eq.'PBASE') then
         if (isub1.ne.9999) goto 910
         oldval = pbase
         call getrv(nin,line,length,iptr,lineno,pbase,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PBASE',pbase,1.0D-8,1.0D-3)
         if (pbase.ne.oldval) then
            clabel = 'Base pressure (Pa),'
            write(nout,*) clabel,varnam(1:8),' = ',pbase
         end if
      else if (varnam(1:varlen).eq.'TN') then
         if (isub1.ne.9999) goto 910
         oldval = tn
         call getrv(nin,line,length,iptr,lineno,tn,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TN',tn,1.0D0,1.0D3)
         if (tn.ne.oldval) then
            clabel = 'Neutral gas temp in chamber (K),'
            write(nout,*) clabel,varnam(1:8),' = ',tn
         end if
      else if (varnam(1:varlen).eq.'RAT') then
         if (isub1.ne.9999) goto 910
         oldval = rat
         call getrv(nin,line,length,iptr,lineno,rat,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RAT',rat,1.0D-10,1.0D-6)
         if (rat.ne.oldval) then
            clabel = 'Plas chamber wall outgas rate (Pa-m/s),'
            write(nout,*) clabel,varnam(1:8),' = ',rat
         end if
      else if (varnam(1:varlen).eq.'NTYPE') then
         if (isub1.ne.9999) goto 910
         ioldvl = ntype
         call getiv(nin,line,length,iptr,lineno,ntype,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'NTYPE',ntype,0,1)
         if (ntype.ne.ioldvl) then
            clabel = 'Pump type,'
            write(nout,*) clabel,varnam(1:8),' = ',ntype
            if (ntype.eq.0) then
               write(nout,*) '     (Turbomolecular pump)'
            else
               write(nout,*) '     (Cryopump)'
            end if
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block VACCY'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block VACCY'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RDNL23(lineno,icode)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to handle the NAMELIST input data for block 'STELLA'.
C
C  Reads data in NAMELIST format
C
C  $STELLA
C  ...
C  $END
C
C  for the NAMELIST block:
C
C      namelist/stella/
C     +     
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Initial version
C
C--Arguments
C  lineno : (IN/OUT) Line number being read in
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'stella.h'
      INCLUDE 'vaccom.h'

C  Arguments
      INTEGER icode,lineno

C  Local variables
      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam
      DOUBLE PRECISION oldval,rval
      INTEGER errlen,ioldvl,iost,iptr,isub1,isub2,ival,length,varlen

C  External functions
      INTEGER  lenstr
      EXTERNAL lenstr

C  External routines
      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Read next line of NAMELIST data

 10   continue
      read(nin,'(a)',iostat=iost) line

C *** On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

C *** Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

C *** Ignore comments

      if (line(1:1).eq.'*') goto 10

C *** If $END, return

      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

C *** Length of line in characters (excluding trailing spaces)

      length = lenstr(line,len(line))
      iptr = 1

C *** It's not $END, so it must be an assignment statement
C *** Get the variable name

      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

C *** Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'PRDIV') then
         if (isub1.ne.9999) goto 910
         oldval = prdiv
         call getrv(nin,line,length,iptr,lineno,prdiv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PRDIV',prdiv,0.0D0,10.0D0)
         if (prdiv.ne.oldval) then
            clabel = 'Divertor chamber pressure in burn (Pa),'
            write(nout,*) clabel,varnam(1:8),' = ',prdiv
         end if
      else if (varnam(1:varlen).eq.'ISTHTR') then
         if (isub1.ne.9999) goto 910
         ioldvl = isthtr
         call getiv(nin,line,length,iptr,lineno,isthtr,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'ISTHTR',isthtr,1,3)
         if (isthtr.ne.ioldvl) then
            clabel = 'Method of auxiliary heating,'
            write(nout,*) clabel,varnam(1:8),' = ',isthtr
            if (isthtr.eq.1) then
               write(nout,*) '     (ECRH)'
            else if (isthtr.eq.2) then
               write(nout,*) '     (LH)'
            else
               write(nout,*) '     (NBI)'
            end if
         end if

      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block STELLA'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block STELLA'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RDNL24(lineno,icode)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to handle the NAMELIST input data for block 'IFE'.
C
C  Reads data in NAMELIST format
C
C  $IFE
C  ...
C  $END
C
C  for the NAMELIST block:
C
C      namelist/ife/
C     +     bldr,bldzl,bldzu,blmatf,cdriv0,cdriv1,cdriv2,chdzl,chdzu,
C     +     chmatf,chrad,dcdrv0,dcdrv1,dcdrv2,drveff,edrive,etave,
C     +     fbreed,fburn,flirad,frrmax,fwdr,fwdzl,fwdzu,fwmatf,gainve,
C     +     ifedrv,ifetyp,mcdriv,pdrive,pifecr,ptargf,rrmax,shdr,shdzl,
C     +     shdzu,shmatf,sombdr,somtdr,tgain,uccarb,ucconc,ucflib,
C     +     uctarg,v1dr,v1dzl,v1dzu,v1matf,v2dr,v2dzl,v2dzu,v2matf,
C     +     v3dr,v3dzl,v3dzu,v3matf
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  21 March 1997
C
C--Reference
C  F/MI/PJK/LOGBOOK12, p.91
C  
C--History
C  21/03/97 PJK 1.000 Initial version
C
C--Arguments
C  lineno : (IN/OUT) Line number being read in
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'ife.h'

C  Arguments
      INTEGER icode,lineno

C  Local variables
      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam
      DOUBLE PRECISION oldval,rval
      INTEGER errlen,ioldvl,iost,iptr,isub1,isub2,ival,length,varlen,i

C  External functions
      INTEGER  lenstr
      EXTERNAL lenstr

C  External routines
      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Read next line of NAMELIST data

 10   continue
      read(nin,'(a)',iostat=iost) line

C *** On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

C *** Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

C *** Ignore comments

      if (line(1:1).eq.'*') goto 10

C *** If $END, return

      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

C *** Length of line in characters (excluding trailing spaces)

      length = lenstr(line,len(line))
      iptr = 1

C *** It's not $END, so it must be an assignment statement
C *** Get the variable name

      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

C *** Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'BLDR') then
         if (isub1.ne.9999) goto 910
         oldval = bldr
         call getrv(nin,line,length,iptr,lineno,bldr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BLDR',bldr,0.0D0,10.0D0)
         if (bldr.ne.oldval) then
            clabel = 'IFE blanket radial thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',bldr
         end if
      else if (varnam(1:varlen).eq.'BLDZL') then
         if (isub1.ne.9999) goto 910
         oldval = bldzl
         call getrv(nin,line,length,iptr,lineno,bldzl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BLDZL',bldzl,0.0D0,10.0D0)
         if (bldzl.ne.oldval) then
            clabel = 'IFE blanket bottom part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',bldzl
         end if
      else if (varnam(1:varlen).eq.'BLDZU') then
         if (isub1.ne.9999) goto 910
         oldval = bldzu
         call getrv(nin,line,length,iptr,lineno,bldzu,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'BLDZU',bldzu,0.0D0,10.0D0)
         if (bldzu.ne.oldval) then
            clabel = 'IFE blanket top part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',bldzu
         end if
      else if (varnam(1:varlen).eq.'BLMATF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'BLMATF',rval,0.0D0,1.0D0)
            blmatf(isub1,isub2) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
c           N.B. isub2 should be in range 0 to MAXMAT
            i = 0
 100        continue
            I = I + 1
            ISUB1 = MOD(I,3)
            IF (ISUB1.EQ.0) ISUB1 = 3
            ISUB2 = INT((I-1)/3)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'BLMATF',rval,0.0D0,1.0D0)
            blmatf(isub1,isub2) = rval
            goto 100
         end if
      else if (varnam(1:varlen).eq.'CDRIV0') then
         if (isub1.ne.9999) goto 910
         oldval = cdriv0
         call getrv(nin,line,length,iptr,lineno,cdriv0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CDRIV0',cdriv0,50.0D0,500.0D0)
         if (cdriv0.ne.oldval) then
            clabel = 'IFE driver cost offset (M$),'
            write(nout,*) clabel,varnam(1:8),' = ',cdriv0
         end if
      else if (varnam(1:varlen).eq.'CDRIV1') then
         if (isub1.ne.9999) goto 910
         oldval = cdriv1
         call getrv(nin,line,length,iptr,lineno,cdriv1,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CDRIV1',cdriv1,50.0D0,500.0D0)
         if (cdriv1.ne.oldval) then
            clabel = 'IFE driver cost offset (M$),'
            write(nout,*) clabel,varnam(1:8),' = ',cdriv1
         end if
      else if (varnam(1:varlen).eq.'CDRIV2') then
         if (isub1.ne.9999) goto 910
         oldval = cdriv2
         call getrv(nin,line,length,iptr,lineno,cdriv2,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CDRIV2',cdriv2,50.0D0,500.0D0)
         if (cdriv2.ne.oldval) then
            clabel = 'IFE driver cost offset (M$),'
            write(nout,*) clabel,varnam(1:8),' = ',cdriv2
         end if
      else if (varnam(1:varlen).eq.'CHDZL') then
         if (isub1.ne.9999) goto 910
         oldval = chdzl
         call getrv(nin,line,length,iptr,lineno,chdzl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CHDZL',chdzl,0.0D0,10.0D0)
         if (chdzl.ne.oldval) then
            clabel = 'IFE chamber bottom part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',chdzl
         end if
      else if (varnam(1:varlen).eq.'CHDZU') then
         if (isub1.ne.9999) goto 910
         oldval = chdzu
         call getrv(nin,line,length,iptr,lineno,chdzu,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CHDZU',chdzu,0.0D0,10.0D0)
         if (chdzu.ne.oldval) then
            clabel = 'IFE chamber top part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',chdzu
         end if
      else if (varnam(1:varlen).eq.'CHMATF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'CHMATF',rval,0.0D0,1.0D0)
            chmatf(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
c           N.B. CHMATF(0) is the first element
            isub1 = 0
 110        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'CHMATF',rval,0.0D0,1.0D0)
            chmatf(isub1) = rval
            isub1 = isub1 + 1
            goto 110
         end if
      else if (varnam(1:varlen).eq.'CHRAD') then
         if (isub1.ne.9999) goto 910
         oldval = chrad
         call getrv(nin,line,length,iptr,lineno,chrad,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'CHRAD',chrad,0.1D0,20.0D0)
         if (chrad.ne.oldval) then
            clabel = 'IFE chamber radial thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',chrad
         end if
      else if (varnam(1:varlen).eq.'DCDRV0') then
         if (isub1.ne.9999) goto 910
         oldval = dcdrv0
         call getrv(nin,line,length,iptr,lineno,dcdrv0,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DCDRV0',dcdrv0,0.0D0,200.0D0)
         if (dcdrv0.ne.oldval) then
            clabel = 'IFE driver cost gradient (M$/MJ),'
            write(nout,*) clabel,varnam(1:8),' = ',dcdrv0
         end if
      else if (varnam(1:varlen).eq.'DCDRV1') then
         if (isub1.ne.9999) goto 910
         oldval = dcdrv1
         call getrv(nin,line,length,iptr,lineno,dcdrv1,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DCDRV1',dcdrv1,0.0D0,200.0D0)
         if (dcdrv1.ne.oldval) then
            clabel = 'IFE driver cost gradient (M$/MJ),'
            write(nout,*) clabel,varnam(1:8),' = ',dcdrv1
         end if
      else if (varnam(1:varlen).eq.'DCDRV2') then
         if (isub1.ne.9999) goto 910
         oldval = dcdrv2
         call getrv(nin,line,length,iptr,lineno,dcdrv2,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DCDRV2',dcdrv2,0.0D0,200.0D0)
         if (dcdrv2.ne.oldval) then
            clabel = 'IFE driver cost gradient (M$/MJ),'
            write(nout,*) clabel,varnam(1:8),' = ',dcdrv2
         end if
      else if (varnam(1:varlen).eq.'DRVEFF') then
         if (isub1.ne.9999) goto 910
         oldval = drveff
         call getrv(nin,line,length,iptr,lineno,drveff,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'DRVEFF',drveff,0.01D0,1.0D0)
         if (drveff.ne.oldval) then
            clabel = 'IFE driver efficiency,'
            write(nout,*) clabel,varnam(1:8),' = ',drveff
         end if
      else if (varnam(1:varlen).eq.'EDRIVE') then
         if (isub1.ne.9999) goto 910
         oldval = edrive
         call getrv(nin,line,length,iptr,lineno,edrive,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'EDRIVE',edrive,0.1D6,50.0D6)
         if (edrive.ne.oldval) then
            clabel = 'IFE driver energy (J),'
            write(nout,*) clabel,varnam(1:8),' = ',edrive
         end if
      else if (varnam(1:varlen).eq.'ETAVE') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'ETAVE',rval,0.01D0,1.0D0)
            etave(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 120        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'ETAVE',rval,0.01D0,1.0D0)
            etave(isub1) = rval
            isub1 = isub1 + 1
            goto 120
         end if
      else if (varnam(1:varlen).eq.'FBREED') then
         if (isub1.ne.9999) goto 910
         oldval = fbreed
         call getrv(nin,line,length,iptr,lineno,fbreed,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBREED',fbreed,0.0D0,0.999D0)
         if (fbreed.ne.oldval) then
            clabel = 'Fraction of breeder outside core,'
            write(nout,*) clabel,varnam(1:8),' = ',fbreed
         end if
      else if (varnam(1:varlen).eq.'FBURN') then
         if (isub1.ne.9999) goto 910
         oldval = fburn
         call getrv(nin,line,length,iptr,lineno,fburn,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FBURN',fburn,0.01D0,1.0D0)
         if (fburn.ne.oldval) then
            clabel = 'IFE burn fraction,'
            write(nout,*) clabel,varnam(1:8),' = ',fburn
         end if
      else if (varnam(1:varlen).eq.'FLIRAD') then
         if (isub1.ne.9999) goto 910
         oldval = flirad
         call getrv(nin,line,length,iptr,lineno,flirad,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FLIRAD',flirad,0.0D0,10.0D0)
         if (flirad.ne.oldval) then
            clabel = 'Radius of FLiBe inlet (HYLIFE) (m),'
            write(nout,*) clabel,varnam(1:8),' = ',flirad
         end if
      else if (varnam(1:varlen).eq.'FRRMAX') then
         if (isub1.ne.9999) goto 910
         oldval = frrmax
         call getrv(nin,line,length,iptr,lineno,frrmax,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FRRMAX',frrmax,1.0D-6,1.0D0)
         if (frrmax.ne.oldval) then
            clabel = 'F-value for IFE repetition rate,'
            write(nout,*) clabel,varnam(1:8),' = ',frrmax
         end if
      else if (varnam(1:varlen).eq.'FWDR') then
         if (isub1.ne.9999) goto 910
         oldval = fwdr
         call getrv(nin,line,length,iptr,lineno,fwdr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FWDR',fwdr,0.0D0,10.0D0)
         if (fwdr.ne.oldval) then
            clabel = 'IFE first wall radial thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',fwdr
         end if
      else if (varnam(1:varlen).eq.'FWDZL') then
         if (isub1.ne.9999) goto 910
         oldval = fwdzl
         call getrv(nin,line,length,iptr,lineno,fwdzl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FWDZL',fwdzl,0.0D0,10.0D0)
         if (fwdzl.ne.oldval) then
            clabel = 'IFE first wall bottom part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',fwdzl
         end if
      else if (varnam(1:varlen).eq.'FWDZU') then
         if (isub1.ne.9999) goto 910
         oldval = fwdzu
         call getrv(nin,line,length,iptr,lineno,fwdzu,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'FWDZU',fwdzu,0.0D0,10.0D0)
         if (fwdzu.ne.oldval) then
            clabel = 'IFE first wall top part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',fwdzu
         end if
      else if (varnam(1:varlen).eq.'FWMATF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'FWMATF',rval,0.0D0,1.0D0)
            fwmatf(isub1,isub2) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
c           N.B. isub2 should be in range 0 to MAXMAT
            i = 0
 130        continue
            I = I + 1
            ISUB1 = MOD(I,3)
            IF (ISUB1.EQ.0) ISUB1 = 3
            ISUB2 = INT((I-1)/3)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'FWMATF',rval,0.0D0,1.0D0)
            fwmatf(isub1,isub2) = rval
            goto 130
         end if
      else if (varnam(1:varlen).eq.'GAINVE') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'GAINVE',rval,1.0D0,500.0D0)
            gainve(isub1) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
            isub1 = 1
 140        continue
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'GAINVE',rval,1.0D0,500.0D0)
            gainve(isub1) = rval
            isub1 = isub1 + 1
            goto 140
         end if
      else if (varnam(1:varlen).eq.'IFEDRV') then
         if (isub1.ne.9999) goto 910
         ioldvl = IFEDRV
         call getiv(nin,line,length,iptr,lineno,IFEDRV,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IFEDRV',IFEDRV,-1,2)
         if (IFEDRV.ne.ioldvl) then
            clabel = 'IFE driver type,'
            write(nout,*) clabel,varnam(1:8),' = ',IFEDRV
            if (IFEDRV.eq.-1) then
               write(nout,*) '     (generic)'
            else if (IFEDRV.eq.0) then
               write(nout,*) '     (generic - fixed)'
            else if (IFEDRV.eq.1) then
               write(nout,*) '     (SOMBRERO laser)'
            else
               write(nout,*) '     (OSIRIS heavy ion beam)'
            end if
         end if
      else if (varnam(1:varlen).eq.'IFETYP') then
         if (isub1.ne.9999) goto 910
         ioldvl = ifetyp
         call getiv(nin,line,length,iptr,lineno,ifetyp,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IFETYP',ifetyp,0,3)
         if (ifetyp.ne.ioldvl) then
            clabel = 'IFE device build type,'
            write(nout,*) clabel,varnam(1:8),' = ',ifetyp
            if (ifetyp.eq.0) then
               write(nout,*) '     (generic)'
            else if (ifetyp.eq.1) then
               write(nout,*) '     (OSIRIS)'
            else if (ifetyp.eq.2) then
               write(nout,*) '     (SOMBRERO)'
            else
               write(nout,*) '     (HYLIFE-II)'
            end if
         end if
      else if (varnam(1:varlen).eq.'MCDRIV') then
         if (isub1.ne.9999) goto 910
         oldval = mcdriv
         call getrv(nin,line,length,iptr,lineno,mcdriv,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'MCDRIV',mcdriv,0.1D0,10.0D0)
         if (mcdriv.ne.oldval) then
            clabel = 'IFE driver cost multiplier,'
            write(nout,*) clabel,varnam(1:8),' = ',mcdriv
         end if
      else if (varnam(1:varlen).eq.'PDRIVE') then
         if (isub1.ne.9999) goto 910
         oldval = pdrive
         call getrv(nin,line,length,iptr,lineno,pdrive,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PDRIVE',pdrive,1.0D6,200.0D6)
         if (pdrive.ne.oldval) then
            clabel = 'IFE driver power to target (W),'
            write(nout,*) clabel,varnam(1:8),' = ',pdrive
         end if
      else if (varnam(1:varlen).eq.'PIFECR') then
         if (isub1.ne.9999) goto 910
         oldval = pifecr
         call getrv(nin,line,length,iptr,lineno,pifecr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PIFECR',pifecr,0.0D0,100.0D0)
         if (pifecr.ne.oldval) then
            clabel = 'IFE cryogenic power (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',pifecr
         end if
      else if (varnam(1:varlen).eq.'PTARGF') then
         if (isub1.ne.9999) goto 910
         oldval = ptargf
         call getrv(nin,line,length,iptr,lineno,ptargf,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'PTARGF',ptargf,0.1D0,100.0D0)
         if (ptargf.ne.oldval) then
            clabel = 'IFE target factory power at 6Hz (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',ptargf
         end if
      else if (varnam(1:varlen).eq.'RRMAX') then
         if (isub1.ne.9999) goto 910
         oldval = rrmax
         call getrv(nin,line,length,iptr,lineno,rrmax,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'RRMAX',rrmax,1.0D0,50.0D0)
         if (rrmax.ne.oldval) then
            clabel = 'Maximum IFE repetition rate (Hz),'
            write(nout,*) clabel,varnam(1:8),' = ',rrmax
         end if
      else if (varnam(1:varlen).eq.'SHDR') then
         if (isub1.ne.9999) goto 910
         oldval = shdr
         call getrv(nin,line,length,iptr,lineno,shdr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SHDR',shdr,0.0D0,10.0D0)
         if (shdr.ne.oldval) then
            clabel = 'IFE shield radial thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',shdr
         end if
      else if (varnam(1:varlen).eq.'SHDZL') then
         if (isub1.ne.9999) goto 910
         oldval = shdzl
         call getrv(nin,line,length,iptr,lineno,shdzl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SHDZL',shdzl,0.0D0,10.0D0)
         if (shdzl.ne.oldval) then
            clabel = 'IFE shield bottom part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',shdzl
         end if
      else if (varnam(1:varlen).eq.'SHDZU') then
         if (isub1.ne.9999) goto 910
         oldval = shdzu
         call getrv(nin,line,length,iptr,lineno,shdzu,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SHDZU',shdzu,0.0D0,10.0D0)
         if (shdzu.ne.oldval) then
            clabel = 'IFE shield top part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',shdzu
         end if
      else if (varnam(1:varlen).eq.'SHMATF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'SHMATF',rval,0.0D0,1.0D0)
            shmatf(isub1,isub2) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
c           N.B. isub2 should be in range 0 to MAXMAT
            i = 0
 150        continue
            I = I + 1
            ISUB1 = MOD(I,3)
            IF (ISUB1.EQ.0) ISUB1 = 3
            ISUB2 = INT((I-1)/3)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'SHMATF',rval,0.0D0,1.0D0)
            shmatf(isub1,isub2) = rval
            goto 150
         end if
      else if (varnam(1:varlen).eq.'SOMBDR') then
         if (isub1.ne.9999) goto 910
         oldval = sombdr
         call getrv(nin,line,length,iptr,lineno,sombdr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SOMBDR',sombdr,0.0D0,10.0D0)
         if (sombdr.ne.oldval) then
            clabel = 'Radius of SOMBRERO blanket bottom (m),'
            write(nout,*) clabel,varnam(1:8),' = ',sombdr
         end if
      else if (varnam(1:varlen).eq.'SOMTDR') then
         if (isub1.ne.9999) goto 910
         oldval = somtdr
         call getrv(nin,line,length,iptr,lineno,somtdr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'SOMTDR',somtdr,0.0D0,10.0D0)
         if (somtdr.ne.oldval) then
            clabel = 'Radius of SOMBRERO blanket top (m),'
            write(nout,*) clabel,varnam(1:8),' = ',somtdr
         end if
      else if (varnam(1:varlen).eq.'TGAIN') then
         if (isub1.ne.9999) goto 910
         oldval = tgain
         call getrv(nin,line,length,iptr,lineno,tgain,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'TGAIN',tgain,1.0D0,500.0D0)
         if (tgain.ne.oldval) then
            clabel = 'IFE target gain,'
            write(nout,*) clabel,varnam(1:8),' = ',tgain
         end if
      else if (varnam(1:varlen).eq.'UCCARB') then
         if (isub1.ne.9999) goto 910
         oldval = uccarb
         call getrv(nin,line,length,iptr,lineno,uccarb,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCCARB',uccarb,10.0D0,1.0D3)
         if (uccarb.ne.oldval) then
            clabel = 'Cost of carbon cloth ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',uccarb
         end if
      else if (varnam(1:varlen).eq.'UCCONC') then
         if (isub1.ne.9999) goto 910
         oldval = ucconc
         call getrv(nin,line,length,iptr,lineno,ucconc,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCCONC',ucconc,0.1D0,1.0D3)
         if (ucconc.ne.oldval) then
            clabel = 'Cost of concrete ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucconc
         end if
      else if (varnam(1:varlen).eq.'UCFLIB') then
         if (isub1.ne.9999) goto 910
         oldval = ucflib
         call getrv(nin,line,length,iptr,lineno,ucflib,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCFLIB',ucflib,10.0D0,1.0D3)
         if (ucflib.ne.oldval) then
            clabel = 'Cost of FLiBe ($/kg),'
            write(nout,*) clabel,varnam(1:8),' = ',ucflib
         end if
      else if (varnam(1:varlen).eq.'UCTARG') then
         if (isub1.ne.9999) goto 910
         oldval = uctarg
         call getrv(nin,line,length,iptr,lineno,uctarg,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCTARG',uctarg,0.1D0,1.0D3)
         if (uctarg.ne.oldval) then
            clabel = 'Cost per IFE target ($/target),'
            write(nout,*) clabel,varnam(1:8),' = ',uctarg
         end if
      else if (varnam(1:varlen).eq.'V1DR') then
         if (isub1.ne.9999) goto 910
         oldval = v1dr
         call getrv(nin,line,length,iptr,lineno,v1dr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'V1DR',v1dr,0.0D0,10.0D0)
         if (v1dr.ne.oldval) then
            clabel = 'IFE void 1 radial thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',v1dr
         end if
      else if (varnam(1:varlen).eq.'V1DZL') then
         if (isub1.ne.9999) goto 910
         oldval = v1dzl
         call getrv(nin,line,length,iptr,lineno,v1dzl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'V1DZL',v1dzl,0.0D0,10.0D0)
         if (v1dzl.ne.oldval) then
            clabel = 'IFE void 1 bottom part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',v1dzl
         end if
      else if (varnam(1:varlen).eq.'V1DZU') then
         if (isub1.ne.9999) goto 910
         oldval = v1dzu
         call getrv(nin,line,length,iptr,lineno,v1dzu,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'V1DZU',v1dzu,0.0D0,10.0D0)
         if (v1dzu.ne.oldval) then
            clabel = 'IFE void 1 top part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',v1dzu
         end if
      else if (varnam(1:varlen).eq.'V1MATF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'V1MATF',rval,0.0D0,1.0D0)
            v1matf(isub1,isub2) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
c           N.B. isub2 should be in range 0 to MAXMAT
            i = 0
 160        continue
            I = I + 1
            ISUB1 = MOD(I,3)
            IF (ISUB1.EQ.0) ISUB1 = 3
            ISUB2 = INT((I-1)/3)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'V1MATF',rval,0.0D0,1.0D0)
            v1matf(isub1,isub2) = rval
            goto 160
         end if
      else if (varnam(1:varlen).eq.'V2DR') then
         if (isub1.ne.9999) goto 910
         oldval = v2dr
         call getrv(nin,line,length,iptr,lineno,v2dr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'V2DR',v2dr,0.0D0,10.0D0)
         if (v2dr.ne.oldval) then
            clabel = 'IFE void 2 radial thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',v2dr
         end if
      else if (varnam(1:varlen).eq.'V2DZL') then
         if (isub1.ne.9999) goto 910
         oldval = v2dzl
         call getrv(nin,line,length,iptr,lineno,v2dzl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'V2DZL',v2dzl,0.0D0,10.0D0)
         if (v2dzl.ne.oldval) then
            clabel = 'IFE void 2 bottom part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',v2dzl
         end if
      else if (varnam(1:varlen).eq.'V2DZU') then
         if (isub1.ne.9999) goto 910
         oldval = v2dzu
         call getrv(nin,line,length,iptr,lineno,v2dzu,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'V2DZU',v2dzu,0.0D0,10.0D0)
         if (v2dzu.ne.oldval) then
            clabel = 'IFE void 2 top part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',v2dzu
         end if
      else if (varnam(1:varlen).eq.'V2MATF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'V2MATF',rval,0.0D0,1.0D0)
            v2matf(isub1,isub2) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
c           N.B. isub2 should be in range 0 to MAXMAT
            i = 0
 170        continue
            I = I + 1
            ISUB1 = MOD(I,3)
            IF (ISUB1.EQ.0) ISUB1 = 3
            ISUB2 = INT((I-1)/3)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'V2MATF',rval,0.0D0,1.0D0)
            v2matf(isub1,isub2) = rval
            goto 170
         end if
      else if (varnam(1:varlen).eq.'V3DR') then
         if (isub1.ne.9999) goto 910
         oldval = v3dr
         call getrv(nin,line,length,iptr,lineno,v3dr,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'V3DR',v3dr,0.0D0,50.0D0)
         if (v3dr.ne.oldval) then
            clabel = 'IFE void 3 radial thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',v3dr
         end if
      else if (varnam(1:varlen).eq.'V3DZL') then
         if (isub1.ne.9999) goto 910
         oldval = v3dzl
         call getrv(nin,line,length,iptr,lineno,v3dzl,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'V3DZL',v3dzl,0.0D0,30.0D0)
         if (v3dzl.ne.oldval) then
            clabel = 'IFE void 3 bottom part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',v3dzl
         end if
      else if (varnam(1:varlen).eq.'V3DZU') then
         if (isub1.ne.9999) goto 910
         oldval = v3dzu
         call getrv(nin,line,length,iptr,lineno,v3dzu,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'V3DZU',v3dzu,0.0D0,30.0D0)
         if (v3dzu.ne.oldval) then
            clabel = 'IFE void 3 top part thickness (m),'
            write(nout,*) clabel,varnam(1:8),' = ',v3dzu
         end if
      else if (varnam(1:varlen).eq.'V3MATF') then
         if (isub1.ne.9999) then
c           here, there was a subscript
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.ne.0) goto 900
            call ranger(nout,'V3MATF',rval,0.0D0,1.0D0)
            v3matf(isub1,isub2) = rval
         else
c           here, there was no subscript.
c           read values until next variable or $end
c           N.B. isub2 should be in range 0 to MAXMAT
            i = 0
 180        continue
            I = I + 1
            ISUB1 = MOD(I,3)
            IF (ISUB1.EQ.0) ISUB1 = 3
            ISUB2 = INT((I-1)/3)
            call getrv(nin,line,length,iptr,lineno,rval,icode)
            if (icode.eq.-1) goto 20
            if (icode.ne.0) goto 900
            call ranger(nout,'V3MATF',rval,0.0D0,1.0D0)
            v3matf(isub1,isub2) = rval
            goto 180
         end if
      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block IFE'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block IFE'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RDNL25(lineno,icode)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to handle the NAMELIST input data for block 'HPLANT'.
C
C  Reads data in NAMELIST format
C
C  $HPLANT
C  ...
C  $END
C
C  for the NAMELIST block:
C
C      namelist/hplant/
C     +     ihplant,
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.6368
C
C--Date
C  22 May 2007
C
C--Reference
C  F/MI/PJK/LOGBOOK20, pp.2,3
C  
C--History
C  22/05/07 PJK 1.000 Initial version
C
C--Arguments
C  lineno : (IN/OUT) Line number being read in
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'cost.h'
      INCLUDE 'htpwr.h'

C  Arguments
      INTEGER icode,lineno

C  Local variables
      CHARACTER clabel*40,clbl*38,clbl2*30
      CHARACTER*200 error,line
      CHARACTER*32 varnam
      DOUBLE PRECISION oldval,rval
      INTEGER errlen,ioldvl,iost,iptr,isub1,isub2,ival,length,varlen,i

C  External functions
      INTEGER  lenstr
      EXTERNAL lenstr

C  External routines
      EXTERNAL getvar,getrv,getiv,nlerr,ranger,rangei

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Read next line of NAMELIST data

 10   continue
      read(nin,'(a)',iostat=iost) line

C *** On error or end, leave routine
      if (iost.ne.0) goto 930

      lineno = lineno + 1

C *** Ignore blank lines
 20   continue
      if (line.eq.' ') goto 10

C *** Ignore comments

      if (line(1:1).eq.'*') goto 10

C *** If $END, return

      if ((line(1:4).eq.'$END').or.(line(1:4).eq.'$end')) goto 930

C *** Length of line in characters (excluding trailing spaces)

      length = lenstr(line,len(line))
      iptr = 1

C *** It's not $END, so it must be an assignment statement
C *** Get the variable name

      call getvar(line,length,iptr,varnam,varlen,isub1,isub2)
      if (varlen.eq.0) then
         error = 'Error whilst reading variable name'
         errlen = lenstr(error,len(error))
         call nlerr(line,length,lineno,error,errlen,varnam,varlen)
         goto 930
      end if

C *** Got the variable name, read the associated data

      if (varnam(1:varlen).eq.'ETAHHTEN') then
         if (isub1.ne.9999) goto 910
         oldval = etahhten
         call getrv(nin,line,length,iptr,lineno,etahhten,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAHHTEN',etahhten,0.0D0,1.48D0)
         if (etahhten.ne.oldval) then
            clabel = 'H production efficiency for HTEN,'
            write(nout,*) clabel,varnam(1:8),' = ',etahhten
         end if
      else if (varnam(1:varlen).eq.'ETAHHTEX') then
         if (isub1.ne.9999) goto 910
         oldval = etahhtex
         call getrv(nin,line,length,iptr,lineno,etahhtex,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAHHTEX',etahhtex,0.0D0,1.19D0)
         if (etahhtex.ne.oldval) then
            clabel = 'H production efficiency for HTEX,'
            write(nout,*) clabel,varnam(1:8),' = ',etahhtex
         end if
      else if (varnam(1:varlen).eq.'ETAHLTE') then
         if (isub1.ne.9999) goto 910
         oldval = etahlte
         call getrv(nin,line,length,iptr,lineno,etahlte,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAHLTE',etahlte,0.0D0,1.0D0)
         if (etahlte.ne.oldval) then
            clabel = 'H production efficiency for LTE,'
            write(nout,*) clabel,varnam(1:8),' = ',etahlte
         end if
      else if (varnam(1:varlen).eq.'ETAHTH') then
         if (isub1.ne.9999) goto 910
         oldval = etahth
         call getrv(nin,line,length,iptr,lineno,etahth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'ETAHTH',etahth,0.0D0,1.0D0)
         if (etahth.ne.oldval) then
            clabel = 'H production efficiency for TH,'
            write(nout,*) clabel,varnam(1:8),' = ',etahth
         end if
      else if (varnam(1:varlen).eq.'HELECMW') then
         if (isub1.ne.9999) goto 910
         oldval = helecmw
         call getrv(nin,line,length,iptr,lineno,helecmw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'HELECMW',helecmw,0.0D0,8000.0D0)
         if (helecmw.ne.oldval) then
            clabel = 'Electrical power for H production (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',helecmw
         end if
      else if (varnam(1:varlen).eq.'HTHERMMW') then
         if (isub1.ne.9999) goto 910
         oldval = hthermmw
         call getrv(nin,line,length,iptr,lineno,hthermmw,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'HTHERMMW',hthermmw,0.0D0,8000.0D0)
         if (hthermmw.ne.oldval) then
            clabel = 'Thermal power for H production (MW),'
            write(nout,*) clabel,varnam(1:8),' = ',hthermmw
         end if
      else if (varnam(1:varlen).eq.'IHPLANT') then
         if (isub1.ne.9999) goto 910
         ioldvl = IHPLANT
         call getiv(nin,line,length,iptr,lineno,IHPLANT,icode)
         if (icode.ne.0) goto 900
         call rangei(nout,'IHPLANT',IHPLANT,0,4)
         if (IHPLANT.ne.ioldvl) then
            clabel = 'Hydrogen Production Plant,'
            write(nout,*) clabel,varnam(1:8),' = ',IHPLANT
            if (IHPLANT.eq.0) then
               write(nout,*) '     (off)'
            else if (IHPLANT.eq.1) then
               write(nout,*) '     (Low Temperature Electrolysis)'
            else if (IHPLANT.eq.2) then
               write(nout,*) '     (High Temp Electrolysis - Endo)'
            else if (IHPLANT.eq.3) then
               write(nout,*) '     (High Temp Electrolysis - Exo)'
            else
               write(nout,*) '     (Thermo-chemical)'
            end if
         end if
      else if (varnam(1:varlen).eq.'UCHHTEN') then
         if (isub1.ne.9999) goto 910
         oldval = uchhten
         call getrv(nin,line,length,iptr,lineno,uchhten,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCHHTEN',uchhten,0.0D0,2000.0D0)
         if (uchhten.ne.oldval) then
            clabel = 'Unit cost of HTEN H production ($/kW),'
            write(nout,*) clabel,varnam(1:8),' = ',uchhten
         end if
      else if (varnam(1:varlen).eq.'UCHHTEX') then
         if (isub1.ne.9999) goto 910
         oldval = uchhtex
         call getrv(nin,line,length,iptr,lineno,uchhtex,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCHHTEX',uchhtex,0.0D0,2000.0D0)
         if (uchhtex.ne.oldval) then
            clabel = 'Unit cost of HTEX H production ($/kW),'
            write(nout,*) clabel,varnam(1:8),' = ',uchhtex
         end if
      else if (varnam(1:varlen).eq.'UCHLTE') then
         if (isub1.ne.9999) goto 910
         oldval = uchlte
         call getrv(nin,line,length,iptr,lineno,uchlte,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCHLTE',uchlte,0.0D0,2000.0D0)
         if (uchlte.ne.oldval) then
            clabel = 'Unit cost of LTE H production ($/kW),'
            write(nout,*) clabel,varnam(1:8),' = ',uchlte
         end if
      else if (varnam(1:varlen).eq.'UCHTH') then
         if (isub1.ne.9999) goto 910
         oldval = uchth
         call getrv(nin,line,length,iptr,lineno,uchth,icode)
         if (icode.ne.0) goto 900
         call ranger(nout,'UCHTH',uchth,0.0D0,2000.0D0)
         if (uchth.ne.oldval) then
            clabel = 'Unit cost of TH H production ($/kW),'
            write(nout,*) clabel,varnam(1:8),' = ',uchth
         end if
      else
         goto 920
      end if

      goto 10

 900  continue
      error = ' Error whilst reading namelist data for block HPLANT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 910  continue
      error = ' Variable has a subscript when it should not'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 920  continue
      error = ' Unknown variable in namelist block HPLANT'
      errlen = lenstr(error,len(error))
      call nlerr(line,length,lineno,error,errlen,varnam,varlen)
      icode = 1
      goto 1000

 930  continue
      icode = 0

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE NLERR(line,length,lineno,error,errlen,string,strlen)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to output an error message due to an illegal namelist
C  item in the input file
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  line   : (INPUT)  Offending line in input file
C  length : (INPUT)  Length of line
C  lineno : (INPUT)  Number of line
C  error  : (INPUT)  Error message
C  errlen : (INPUT)  Length of error message
C  string : (INPUT)  Offending variable name
C  strlen : (INPUT)  Length of variable name
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'

C  Arguments
      CHARACTER*(*) error,line,string
      INTEGER errlen,length,lineno,strlen

C  Local variables
      CHARACTER*256 ctemp
      INTEGER clen

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      if (errlen.eq.0) then
         if (strlen.eq.0) then
            clen = 0
         else
            ctemp = string(1:strlen)
            clen = strlen
         end if
      else
         if (strlen.eq.0) then
            ctemp = error(1:errlen)
            clen = errlen
         else
            ctemp = error(1:errlen) // ' : ' // string(1:strlen)
            clen = errlen + 3 + strlen
         end if
      end if

      if (clen.gt.0) write (nout,'(a)') ctemp(1:clen)
      if (length.gt.0) write (nout,10) lineno,line(1:length)
 10   format (1x,i6,' : ',a)

      close(nout)

      write(*,*) ' '
      write(*,*) 'Error in input file.'
      write(*,*) 'See the output file for more details.'
      write(*,*) ' '
      write(*,*) 'Beware of using spaces before $name and $end.'
      write(*,*) 'PROCESS stopping.'

      stop
      end
C----------------------------------------------------------------------
      SUBROUTINE CTOI(string,length,ivar,icode)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to convert the ASCII digits in STRING(1:LENGTH)
C  to the integer IVAR.
C  Equivalent to 'READ(STRING(1:LENGTH),I) IVAR' but this routine
C  conforms to the ANSI standard.
C
C  Each digit is parsed in turn, the current total is multiplied
C  by ten and the new digit is added.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  string : (INPUT)  Character string containing digits of the number
C  length : (INPUT)  Useful length of character string
C  ivar   : (OUTPUT) Integer containing the value stored in the string
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) string
      INTEGER icode,ivar,length

C  Global variables

C  Local variables
      CHARACTER*200 xstr
      INTEGER iptr,izero,xlen
      LOGICAL negate

C  External functions
      INTEGER  lenstr
      EXTERNAL lenstr

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      ivar = 0
      icode = 0

      if (length.le.0) goto 1000

      negate = .false.
      izero = ichar('0')
      iptr = 1
      xstr = string(1:length)

C *** Ignore trailing spaces

      xlen = lenstr(xstr,length)
      if (xlen.le.0) goto 1000

C *** Ignore leading spaces

 10   continue
      if (xstr(iptr:iptr).eq.' ') then
         iptr = iptr + 1
         if (iptr.gt.xlen) goto 1000
         goto 10
      end if

C *** Check for leading + or -

      if (xstr(iptr:iptr).eq.'+') then
         iptr = iptr + 1
         if (iptr.gt.xlen) goto 1000
      else if (xstr(iptr:iptr).eq.'-') then
         negate = .true.
         iptr = iptr + 1
         if (iptr.gt.xlen) goto 1000
      else
         continue
      end if

C *** Ignore leading zeros

 20   continue
      if (xstr(iptr:iptr).eq.'0') then
         iptr = iptr + 1
         if (iptr.gt.xlen) goto 1000
         goto 20
      end if

C *** Check for number too large

      if ((xlen-iptr+1).gt.10) then
         if (negate) then
            ivar = -2147483648
         else
            ivar = 2147483647
         end if
         icode = 1
         goto 1000
      else if ((xlen-iptr+1).eq.10) then
         if (xstr(iptr:xlen).gt.'2147483647') then
            if (negate) then
               ivar = -2147483648
            else
               ivar = 2147483647
            end if
            icode = 1
            goto 1000
         end if
      else
         continue
      end if

C *** Parse the digits

 30   continue
      if ((xstr(iptr:iptr).ge.'0').and.(xstr(iptr:iptr).le.'9')) then
         ivar = (ivar * 10) + (ichar(xstr(iptr:iptr))-izero)
         iptr = iptr + 1
         if (iptr.le.xlen) goto 30

C *** This is the normal exit path...

         if (negate) ivar = -ivar

      else
         icode = 1
      end if

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE CTOR(string,length,rval,icode)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to convert the ASCII digits in STRING(1:LENGTH)
C  to the real value RVAL.
C
C  Parse the string one character at a time, from the left, handling
C  mantissa, and all other components of the real number separately,
C  combining them at the end.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  string : (INPUT)  Character string containing digits of the number
C  length : (INPUT)  Useful length of character string
C  rval   : (OUTPUT) DBLE variable containing the value stored in string
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) string
      DOUBLE PRECISION rval
      INTEGER length,icode

C  Local variables
      DOUBLE PRECISION valbdp,valadp,xfact
      INTEGER iptr,izero,iexpon
      LOGICAL negatm,negate

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      iptr = 1

C *** Ignore leading spaces

 10   continue
      if (string(iptr:iptr).eq.' ') then
         iptr = iptr + 1
         if (iptr.le.length) goto 10
      end if

C *** Initialise real value

      rval = 0.0D0

C *** ASCII '0'

      izero = ichar('0')

C *** If negative mantissa

      negatm = .false.

C *** If negative exponent

      negate = .false.

C *** Value before decimal point

      valbdp = 0.0D0

C *** Value after decimal point

      valadp = 0.0D0

C *** Exponent

      iexpon = 0

C *** First character can be +, -, ., or <digit>

      if (string(iptr:iptr).eq.'+') then
         iptr = iptr + 1
         if (iptr.gt.length) goto 50
      else if (string(iptr:iptr).eq.'-') then
         iptr = iptr + 1
         if (iptr.gt.length) goto 50
         negatm = .true.
      else
         continue
      end if

C *** Parse the mantissa - before the decimal point

      valbdp = 0.0d0
      xfact = 0.1d0
 20   continue
      if ((string(iptr:iptr).ge.'0').and.
     +     (string(iptr:iptr).le.'9')) then
         valbdp = (valbdp * 10.d0) + 
     +        dble(ichar(string(iptr:iptr))-izero)
         iptr = iptr + 1
         if (iptr.gt.length) goto 50
         goto 20
      end if

C *** After the mantissa, we expect '.' or 'd' or 'e'

      if (string(iptr:iptr).eq.'.') then
         iptr = iptr + 1
         if (iptr.gt.length) goto 50
      end if

C *** Parse the mantissa - after the decimal point

      valadp = 0.0d0
 30   continue
      if ((string(iptr:iptr).ge.'0').and.
     +     (string(iptr:iptr).le.'9')) then
         valadp = valadp + (dble(ichar(string(iptr:iptr))-izero)*xfact)
         xfact = xfact * 0.1d0
         iptr = iptr + 1
         if (iptr.gt.length) goto 50
         goto 30
      end if

C *** Now we expect the exponent

      if ((string(iptr:iptr).eq.'D').or.
     +     (string(iptr:iptr).eq.'E').or.
     +     (string(iptr:iptr).eq.'d').or.
     +     (string(iptr:iptr).eq.'e')) then
         iptr = iptr + 1
         if (iptr.gt.length) goto 50

C *** First character can be +, -, ., or <digit>

         if (string(iptr:iptr).eq.'+') then
            iptr = iptr + 1
            if (iptr.gt.length) goto 50
         else if (string(iptr:iptr).eq.'-') then
            iptr = iptr + 1
            if (iptr.gt.length) goto 50
            negate = .true.
         else
            continue
         end if

C *** Parse the exponent

 40      continue
         if ((string(iptr:iptr).ge.'0').and.
     +        (string(iptr:iptr).le.'9')) then
            iexpon = (iexpon * 10) + (ichar(string(iptr:iptr))-izero)
            iptr = iptr + 1
            if (iptr.le.length) goto 40
         end if
      else
         goto 60
      end if

 50   continue

C *** Negative exponent?

      if (negate) iexpon = -iexpon

C *** Build the number at last

      if (iexpon.eq.0) then
         rval = (valbdp + valadp)
      else
         rval = (valbdp + valadp) * (10.0d0 ** iexpon)
      end if

C *** Negative mantissa?

      if (negatm) rval = -rval

C *** All OK

      icode = 0
      goto 1000

C *** Errors

 60   continue
      icode = 1

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE GETIV(nin,line,length,iptr,lineno,ival,icode)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to extract an integer value from the string LINE
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  nin    : (INPUT)  Fortran input file identifier
C  line   : (INPUT)  String containing present line of input file
C  length : (INPUT)  Length of LINE
C  iptr   : (IN/OUT) Character counter
C  lineno : (INPUT)  Number of current line in input file
C  ival   : (OUTPUT) Extracted integer value
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) line
      INTEGER icode,iptr,ival,length,lineno,nin

C  Local variables
      CHARACTER*200   varval
      INTEGER varlen,iost

C  External functions
      INTEGER  lenstr
      EXTERNAL lenstr

C  External routines
      EXTERNAL ctoi

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Ignore leading spaces

 10   continue
      if (iptr.le.length) then
         if (line(iptr:iptr).eq.' ') then
            iptr = iptr + 1
            goto 10
         end if
      end if

      if (iptr.gt.length) then

C *** Read next line of namelist data

 20      continue
         read(nin,'(a)',iostat=iost) line

C *** On error or end, leave routine with error code set

         if (iost.ne.0) goto 60

         lineno = lineno + 1

C *** Ignore blank lines

         if (line.eq.' ') goto 10

C *** Ignore comments

         if (line(1:1).eq.'*') goto 10

C *** Length of line excluding trailing spaces

         length = lenstr(line,len(line))

C *** If $end, return

         if (line(1:1).eq.'$') then
            icode = -1
            goto 1000
         end if
         iptr = 1
 30      continue
         if (line(iptr:iptr).eq.' ') then
            iptr = iptr + 1
            if (iptr.le.length) goto 30
            goto 20
         end if

C *** A continuation line starts with 0-9, - or + (more numbers)

         if ((line(iptr:iptr).ge.'0').and.(line(iptr:iptr).le.'9'))
     +        goto 40
         if ((line(iptr:iptr).eq.'+').or.(line(iptr:iptr).eq.'-'))
     +        goto 40
         icode = -1
         goto 1000
 40      continue
      end if

C *** Put rest of line into varval (makes it easier to parse)

      varval = line(iptr:)
      varlen = index(varval,',') - 1
      if (varlen.le.0) varlen = index(varval,' ') - 1
      if (varlen.le.0) varlen = iptr

C *** Update pointer

      iptr = iptr + varlen

C *** Ignore trailing spaces

 50   continue
      if (line(iptr:iptr).eq.' ') then
         iptr = iptr + 1
         if (iptr.le.length) goto 50
      end if

C *** Ignore comma, if present

      if (iptr.le.length) then
         if (line(iptr:iptr).eq.',') iptr = iptr + 1
      end if

C *** Convert the ASCII text into an integer value

      call ctoi(varval,varlen,ival,icode)

      goto 1000

 60   continue
      icode = 1

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE GETRV(nin,line,length,iptr,lineno,rval,icode)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to extract a double precision value from the string LINE
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  nin    : (INPUT)  Fortran input file identifier
C  line   : (IN/OUT) String containing present line of input file
C  length : (IN/OUT) Length of LINE
C  iptr   : (IN/OUT) Character counter
C  lineno : (IN/OUT) Number of current line in input file
C  rval   : (OUTPUT) Extracted double precision value
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*200 line
      DOUBLE PRECISION rval
      INTEGER icode,iptr,length,lineno,nin

C  Local variables
      CHARACTER*200 varval
      INTEGER varlen,iost

C  External functions
      INTEGER  lenstr
      EXTERNAL lenstr

C  External routines
      EXTERNAL ctor

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Ignore leading spaces

 10   continue
      if (iptr.le.length) then
         if (line(iptr:iptr).eq.' ') then
            iptr = iptr + 1
            goto 10
         end if
      end if
      if (iptr.gt.length) then

C *** Read next line of namelist data

 20      continue
         read(nin,'(a)',iostat=iost) line

C *** On error or end, leave routine with error set

         if (iost.ne.0) goto 60

         lineno = lineno + 1

C *** Ignore blank lines

         if (line.eq.' ') goto 10

C *** Ignore comments

         if (line(1:1).eq.'*') goto 10

C *** Length of line excluding trailing spaces

         length = lenstr(line,len(line))

C *** If $end, return

         if (line(1:1).eq.'$') then
            icode = -1
            goto 1000
         end if
         iptr = 1
 30      continue
         if (line(iptr:iptr).eq.' ') then
            iptr = iptr + 1
            if (iptr.le.length) goto 30
            goto 20
         end if

C *** A continuation line starts with 0-9, - or + (more numbers)

         if ((line(iptr:iptr).ge.'0').and.(line(iptr:iptr).le.'9'))
     +        goto 40
         if ((line(iptr:iptr).eq.'+').or.(line(iptr:iptr).eq.'-'))
     +        goto 40
         icode = -1
         goto 1000
 40      continue

      end if

C *** Put rest of line into varval (makes it easier to parse)

      varval = line(iptr:)
      varlen = index(varval,',') - 1
      if (varlen.le.0) varlen = index(varval,' ') - 1
      if (varlen.le.0) varlen = iptr

C *** Update pointer

      iptr = iptr + varlen

C *** Ignore trailing spaces

 50   continue
      if (line(iptr:iptr).eq.' ') then
         iptr = iptr + 1
         if (iptr.le.length) goto 50
      end if

C *** Ignore comma, if present

      if (iptr.le.length) then
         if (line(iptr:iptr).eq.',') iptr = iptr + 1
      end if

C *** Convert the ASCII text into a real value

      call ctor(varval,varlen,rval,icode)

      goto 1000

 60   continue
      icode = 1

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE GETSS(line,length,iptr,isub1,isub2,icode)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to extract subscript(s) from string LINE, if present.
C
C  Look at the next non-space character in LINE. If it's a left
C  bracket, assume a subscript and extract it. Otherwise return 9999.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  18 March 1997
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C  18/03/97 PJK 1.100 Modified so that isub1=9999 if there are no
C                     subscripts. Previously could not cope with a
C                     genuine subscript of 0
C
C--Arguments
C  line   : (INPUT)  String containing present line of input file
C  length : (INPUT)  Length of LINE
C  iptr   : (IN/OUT) Character counter
C  isub1  : (OUTPUT) First subscript
C  isub2  : (OUTPUT) Second subscript
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) line
      INTEGER icode,iptr,isub1,isub2,length

C  Local variables
      INTEGER izero
      LOGICAL negate

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Initial values

      isub1 = 9999
      isub2 = 0

C *** First character should be '('

      if (line(iptr:iptr).ne.'(') goto 70
      isub1 = 0
      iptr = iptr + 1
      if (iptr.gt.length) goto 80

C *** Parse the first subscript
C *** Ignore leading spaces

 10   continue
      if (line(iptr:iptr).eq.' ') then
         iptr = iptr + 1
         if (iptr.gt.length) goto 80
         goto 10
      end if

      izero = ichar('0')
      negate = .false.

C *** Extract and evaluate the first subscript
C *** Subscript may be prefaced by '+' or '-'

      if (line(iptr:iptr).eq.'+') then
         iptr = iptr + 1
         if (iptr.gt.length) goto 80
      else if (line(iptr:iptr).eq.'-') then
         negate = .true.
         iptr = iptr + 1
         if (iptr.gt.length) goto 80
      else
         continue
      end if

 20   continue

      if ((line(iptr:iptr).ge.'0').and.(line(iptr:iptr).le.'9')) then
         isub1 = isub1 * 10 + ichar(line(iptr:iptr)) - izero
         iptr = iptr + 1
         if (iptr.gt.length) goto 80
         goto 20
      end if
      if (negate) isub1 = -isub1

C *** Ignore trailing spaces of first subscript

 30   continue
      if (line(iptr:iptr).eq.' ') then
         iptr = iptr + 1
         if (iptr.gt.length) goto 70
         goto 30
      end if

C *** Is there a second subscript?

      if (line(iptr:iptr).eq.',') then
         iptr = iptr + 1
         if (iptr.gt.length) goto 80

C *** Ignore leading spaces of second subscript

 40      continue
         if (line(iptr:iptr).eq.' ') then
            iptr = iptr + 1
            if (iptr.gt.length) goto 80
            goto 40
         end if

C *** Extract and evaluate the second subscript

         negate = .false.

C *** Subscript may be prefaced by '+' or '-'

         if (line(iptr:iptr).eq.'+') then
            iptr = iptr + 1
            if (iptr.gt.length) goto 80
         else if (line(iptr:iptr).eq.'-') then
            negate = .true.
            iptr = iptr + 1
            if (iptr.gt.length) goto 80
         else
            continue
         end if
 50      continue
         if ((line(iptr:iptr).ge.'0').and.(line(iptr:iptr).le.'9')) then
            isub2 = isub2 * 10 + ichar(line(iptr:iptr)) - izero
            iptr = iptr + 1
            if (iptr.gt.length) goto 80
            goto 50
         end if

C *** Is it a negative subscript?

         if (negate) isub2 = -isub2

C *** Ignore trailing spaces of second subscript

 60      continue
         if (line(iptr:iptr).eq.' ') then
            iptr = iptr + 1
            if (iptr.le.length) goto 60
         end if

      end if

C *** Must end with ')'

      if (line(iptr:iptr).ne.')') goto 80
      iptr = iptr + 1

 70   continue
      icode = 0
      goto 1000

 80   continue
      icode = 1

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE GETVAR(line,length,iptr,varnam,varlen,isub1,isub2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to extract a variable name from LINE.
C  Extract also any subscript & ignore the '=' sign. Leave IPTR
C  pointing to the first character of the value to be assigned.
C  Returns VARLEN of 0 if error.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  line   : (INPUT)  String containing present line of input file
C  length : (INPUT)  Length of LINE
C  iptr   : (IN/OUT) Character counter
C  varnam : (OUTPUT) Variable name
C  varlen : (OUTPUT) Length of variable name
C  isub1  : (OUTPUT) First subscript
C  isub2  : (OUTPUT) Second subscript
C  icode  : (OUTPUT) Diagnostic flag
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) line,varnam
      INTEGER icode,iptr,isub1,isub2,length,varlen

C  Local variables
      INTEGER ifrom,ito

C  External routines
      EXTERNAL getss,uppcas

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Convert string to upper case

      call uppcas(line,1,length)

      varlen = 0
      ifrom = iptr

C *** First character must be alphabetic

      if ((line(iptr:iptr).lt.'A').or.(line(iptr:iptr).gt.'Z'))
     +     goto 1000
      iptr = iptr + 1
      if (iptr.gt.length) goto 1000

C *** Now parse the rest of the letters (must be alphanumeric)

 10   continue
      if  (((line(iptr:iptr).ge.'A').and.(line(iptr:iptr).le.'Z')).or.
     +     ((line(iptr:iptr).ge.'0').and.(line(iptr:iptr).le.'9'))) then
         iptr = iptr + 1
         if (iptr.le.length) goto 10
      end if

C *** Extract variable name

      ito = iptr - 1
      varlen = ito - ifrom + 1
      if (varlen.gt.0) varnam = line(ifrom:ito)

C *** Ignore intervening spaces

 20   continue
      if (line(iptr:iptr).eq.' ') then
         iptr = iptr + 1
         if (iptr.le.length) goto 20
      end if

C *** Now extract any subscript

      call getss(line,length,iptr,isub1,isub2,icode)
      if (icode.ne.0) then
         varlen = 0
         goto 1000
      end if

C *** Ignore intervening spaces

 30   continue
      if (line(iptr:iptr).eq.' ') then
         iptr = iptr + 1
         if (iptr.le.length) goto 30
      end if

C *** We now expect '='

      if (line(iptr:iptr).eq.'=') then
         iptr = iptr + 1
      else
         varlen = 0
      end if

 1000 continue

      return
      end
C----------------------------------------------------------------------
      INTEGER FUNCTION LENSTR(buffer,maxlen)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to return the length of string BUFFER. The length is the
C  number of non-space characters counting from the left.
C  It works by performing a binary search to approximate the
C  position of the last non-space character, and then works
C  backwards from there. This is much more efficient than
C  just searching from the end, particularly where MAXLEN is large.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  buffer : (INPUT)  Character string of interest
C  maxlen : (INPUT)  Maximum length of BUFFER
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) buffer
      INTEGER maxlen

C  Global variables

C  Local variables
      INTEGER half,high,i

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Initialise to zero

      lenstr = 0

C *** If null string, return

      if (maxlen.le.0) goto 1000

C *** Is it all spaces?

      if (buffer(1:maxlen).eq.' ') goto 1000

C *** Do binary chop

      high = maxlen

C *** The average length of line is 40 characters

      if (high.ge.64) then
         if (buffer(64:).eq.' ') high = 64
      end if

C *** Do binary chop

 10   continue
      if (high.gt.4) then
         half = (high + 1) / 2
         if (buffer(half:).eq.' ') then
            high = half
            goto 10
         end if
      end if

C *** Now search from the end backwards
      do 20 i = high,1,-1
         lenstr = i
         if (buffer(i:i).ne.' ') goto 1000
 20   continue

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE UPPCAS(string,ifrom,ito)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to convert string(ifrom:ito) to upper case
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  string : (IN/OUT) Character string of interest
C  ifrom  : (INPUT)  Starting character for conversion
C  ito    : (INPUT)  Final character for conversion
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) string
      INTEGER ifrom,ito

C  Local variables
      CHARACTER*1 letter
      CHARACTER*26 upptab
      INTEGER loop,iptr

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      if (ifrom.le.ito) then

         upptab = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

         do 10 loop = ifrom,ito
            letter = string(loop:loop)
            iptr = index('abcdefghijklmnopqrstuvwxyz',letter)
            if (iptr.gt.0) string(loop:loop) = upptab(iptr:iptr)
 10      continue

      end if

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RANGER(nout,cvar,varval,minval,maxval)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to check whether a double precision variable lies within
C  the desired range, and reports an error if it doesn't.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  nout   : (INPUT)  Fortran output unit identifier
C  cvar   : (INPUT)  Character string containing name of variable
C  varval : (INPUT)  Value of variable
C  minval : (INPUT)  Minimum allowed value of variable
C  maxval : (INPUT)  Maximum allowed value of variable
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) cvar
      DOUBLE PRECISION varval,minval,maxval
      INTEGER nout

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      if (minval.ge.maxval) then
         write(nout,*) 'Error in routine RANGER:'
         write(nout,*) 'Illegal relative values of minval and maxval'
         write(nout,*) 'for variable ',cvar
         write(nout,*) 'PROCESS stopping.'
         STOP
      end if

      if ((varval.lt.minval).or.(varval.gt.maxval)) then
         write(nout,*) 'Variable range error.'
         write(nout,*) cvar,' lies outside its allowed range :'
         write(nout,*) ' '
         write(nout,*) 'Minimum value = ',minval
         write(nout,*) 'Maximum value = ',maxval
         write(nout,*) ' '
         write(nout,*) ' Actual value = ',varval
         write(nout,*) ' '
         write(nout,*) 'PROCESS stopping.'
         STOP
      else
         goto 1000
      end if

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RANGEI(nout,cvar,varval,minval,maxval)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to check whether an integer variable lies within
C  the desired range, and reports an error if it doesn't.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  28 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Improved layout
C
C--Arguments
C  nout   : (INPUT)  Fortran output unit identifier
C  cvar   : (INPUT)  Character string containing name of variable
C  varval : (INPUT)  Value of variable
C  minval : (INPUT)  Minimum allowed value of variable
C  maxval : (INPUT)  Maximum allowed value of variable
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) cvar
      INTEGER nout,varval,minval,maxval

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      if (minval.ge.maxval) then
         write(nout,*) 'Error in routine RANGEI:'
         write(nout,*) 'Illegal relative values of minval and maxval'
         write(nout,*) 'for variable ',cvar
         write(nout,*) 'PROCESS stopping.'
         STOP
      end if

      if ((varval.lt.minval).or.(varval.gt.maxval)) then
         write(nout,*) 'Variable range error.'
         write(nout,*) cvar,' lies outside its allowed range :'
         write(nout,*) ' '
         write(nout,*) 'Minimum value = ',minval
         write(nout,*) 'Maximum value = ',maxval
         write(nout,*) ' '
         write(nout,*) ' Actual value = ',varval
         write(nout,*) ' '
         write(nout,*) 'PROCESS stopping.'
         STOP
      else
         goto 1000
      end if

 1000 continue

      return
      end
