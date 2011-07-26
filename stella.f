C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: stella.f,v 1.19 2006/05/25 09:27:02 pknight Exp $
C  Module name    : $RCSfile: stella.f,v $
C  Version no.    : $Revision: 1.19 $
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE STCALL
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.200
C
C--Description
C  Routine to call the physics and engineering modules
C  relevant to stellarators
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.4181
C
C--Date
C  19 May 1999
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Initial version
C  23/01/97 PJK 1.100 Split routine POWER into POWER1 and POWER2
C  19/11/97 PJK 1.101 Corrected call to STCOIL (missing arguments)
C  19/05/99 PJK 1.200 Added call to routine AVAIL
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

C  Local variables

C  External routines
      EXTERNAL acpow,avail,bldgcall,costs,divcall,fwbs,geomty,power1,
     +     power2,stbild,stcoil,stfwbs,stgeom,stphys,ststrc,tfpwr,
     +     vaccall

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      call stgeom
      call geomty

      call stbild(nout,0)

      call stphys

      call stcoil(nout,0)

      call ststrc(nout,0)

      call stfwbs
      call fwbs(nout,0)

      call divcall(0,nout)

      call tfpwr(nout,0)

C+**PJK 23/01/97      call power(nout,0)

      call power1

      call vaccall(nout,0)

      call bldgcall(nout,0)

      call acpow(nout,0)

C+**PJK 23/01/97
      call power2(nout,0)

      call avail(nout,0)

      call costs(nout,0)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STINIT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.041
C
C--Description
C  Routine to initialise the variables relevant to stellarators.
C  Many of these may override the values set in routine INITIAL.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  22 January 1997
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Initial version
C  09/09/94 PJK 1.010 Changed ICASE
C  07/12/94 PJK 1.020 Changed default q and kappa values
C  04/12/95 PJK 1.030 Ensured stellarators do not use D-He3 reaction
C  26/02/96 PJK 1.040 Modified initial setting of ISTELL (moved to
C                     routine DEVTYP)
C  22/01/97 PJK 1.041 Subsumed heattr.h, heatrinp.h and pfelect.h into
C                     htpwr.h
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
      INCLUDE 'blanket.h'
      INCLUDE 'bldgcom.h'
      INCLUDE 'bldgvol.h'
      INCLUDE 'build.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'cost.h'
      INCLUDE 'divrt.h'
      INCLUDE 'estocom.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'ineq.h'
      INCLUDE 'labels.h'
      INCLUDE 'numer.h'
      INCLUDE 'osections.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pulse.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'stella.h'
      INCLUDE 'struccom.h'
      INCLUDE 'sweep.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'times.h'
      INCLUDE 'torsdat.h'
      INCLUDE 'vaccom.h'
      INCLUDE 'vltcom.h'

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      if (istell.eq.0) goto 1000

C *** Numerics quantities

      boundl(1) = 5.0D0

      boundu(1) = 20.0D0
      boundu(3) = 30.0D0
      boundu(29) = 20.0D0

      icase = 'PROCESS stellarator model'

C *** Build quantities

      ohcth = 0.0D0
      iohcl = 0
      ohhghf = 0.0D0
      gapoh = 0.0D0
      bcylth = 0.0D0
      tfootfi = 1.0D0

C *** Physics quantities

      aspect = 12.5D0
      dnbeta = 0.0D0
      rmajor = 20.0D0
      kappa = 2.0D0
      triang = 0.0D0
      q = 1.03D0
      idhe3 = 0

C *** Times for different phases

      tburn = 3.15576D7
      tohs = 0.0D0
      tpulse = 3.15576D7
      tqnch = 0.0D0
      tramp = 0.0D0

C *** TF coil quantities

      tfno = 50.0D0

C *** Stellarator switches

      isthtr = 3

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STGEOM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the plasma volume and surface area
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  29 June 1994
C
C--Reference
C  None
C  
C--History
C  28/06/94 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters

C  INCLUDE files

C  Arguments

C  Global variables

C  Local variables

C  External functions

C  External routines

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC






      END
C----------------------------------------------------------------------
      SUBROUTINE STBILD(nout,iprint)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.010
C
C--Description
C  Routine to determine the build of the stellarator machine
C  Values calculated are based on the mean minor radius, etc.,
C  as the actual radial and vertical build thicknesses vary with
C  toroidal angle.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  10 June 1996
C
C--Reference
C  None
C  
C--History
C  29/06/94 PJK 1.000 Initial version
C  10/06/96 PJK 1.010 Added first wall area calculation
C
C--Arguments
C  nout   : (INPUT)  Fortran output unit identifier
C  iprint : (INPUT)  Switch denoting whether to produce output
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'divrt.h'
      INCLUDE 'build.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER iprint,nout

C  Local variables
      DOUBLE PRECISION drbild,radius

C  External routines
      EXTERNAL obuild,ovarre,oheadr,osubhd

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      if (iprint.eq.1) goto 10

C *** Radial build to centre of plasma (should be equal to rmajor)

      rbld = bore + ohcth + gapoh + bcylth + tfcth + ddwi +
     +     gapds + shldith + blnkith + fwith + scrapli + rminor

C *** Radius to inner edge of inboard shield

      rsldi = rmajor - rminor - scrapli - fwith - blnkith - shldith

C *** Radius to outer edge of outboard shield

      rsldo = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth

C *** Thickness of outboard TF coil legs

      tfthko = tfootfi*tfcth 

C *** Radius to centre of outboard TF coil legs

      gapsto = gapomin
      rtot = rsldo + gapsto + ddwi + 0.5d0 * tfthko

C *** Height to inside edge of TF coil
C *** Roughly equal to average of (inboard build from TF coil to plasma
C *** centre) and (outboard build from plasma centre to TF coil)

      hmax = 0.5D0 * (
     +     (ddwi+gapds+shldith+blnkith+fwith+scrapli+rminor) +
     +     (rminor+scraplo+fwoth+blnkoth+shldoth+gapsto+ddwi) )

C *** Outer divertor strike point radius, set equal to major radius

      rstrko = rmajor

C *** First wall area

      fwarea = 4.0d0*pi**2*sf*rmajor*(rminor+(scrapli+scraplo)/2.d0)
     +     *0.875d0

 10   continue

      if ((iprint.eq.0).or.(sect06.eq.0)) goto 1000

C *** Print out device build

      call oheadr(nout,'Radial Build')

      write(nout,20)
 20   format(t43,'Thickness (m)',t60,'Radius (m)')

      radius = 0.0d0
      call obuild(nout,'Device centreline',0.0D0,radius)

      drbild = bore + ohcth + gapoh + bcylth
      radius = radius + drbild
      call obuild(nout,'Machine bore',drbild,radius)

      radius = radius + tfcth
      call obuild(nout,'TF coil inner leg',tfcth,radius)

      radius = radius + ddwi
      call obuild(nout,'Cryostat',ddwi,radius)

      radius = radius + gapds
      call obuild(nout,'Gap',gapds,radius)

      radius = radius + shldith
      call obuild(nout,'Inboard shield',shldith,radius)

      radius = radius + blnkith
      call obuild(nout,'Inboard blanket',blnkith,radius)

      radius = radius + fwith
      call obuild(nout,'Inboard first wall',fwith,radius)

      radius = radius + scrapli
      call obuild(nout,'Inboard scrape-off',scrapli,radius)

      radius = radius + rminor
      call obuild(nout,'Plasma geometric centre',rminor,radius)

      radius = radius + rminor
      call obuild(nout,'Plasma outer edge',rminor,radius)

      radius = radius + scraplo
      call obuild(nout,'Outboard scrape-off',scraplo,radius)

      radius = radius + fwoth
      call obuild(nout,'Outboard first wall',fwoth,radius)

      radius = radius + blnkoth
      call obuild(nout,'Outboard blanket',blnkoth,radius)

      radius = radius + shldoth
      call obuild(nout,'Outboard shield',shldoth,radius)

      radius = radius + gapsto
      call obuild(nout,'Gap',gapsto,radius)

      radius = radius + ddwi
      call obuild(nout,'Cryostat',ddwi,radius)

      radius = radius + tfthko
      call obuild(nout,'TF coil outer leg',tfthko,radius)

C *** Port size information

      call osubhd(nout,'Port Size Information :')
      call ovarre(nout,'Port width (m)','(prtsz)',prtsz)
      call ovarre(nout,'Port requirement for beams (m)','(prtszreq)',
     +     prtszreq)

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STPHYS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.126
C
C--Description
C  Routine to calculate stellarator plasma physics information
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  16 July 2001
C
C--Reference
C  None
C  
C--History
C  29/06/94 PJK 1.000 Initial version
C  16/01/96 PJK 1.100 Modifications in the light of D-He3 changes
C                     (idhe3 is always set to zero for stellarators)
C  10/06/96 PJK 1.110 Added use of IWALLD in wall load calculation
C  01/04/98 PJK 1.120 Modified BETCOM and PCOND calls
C  01/04/98 PJK 1.121 Modified BETCOM call
C  24/04/98 PJK 1.122 Modified BETCOM call
C  30/06/98 PJK 1.123 Modified PCOND call
C  19/01/99 PJK 1.124 Modified PCOND call
C  16/07/01 PJK 1.125 Modified PCOND call
C  22/05/06 PJK 1.126 Modified PALPH2 call
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
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'times.h'
      INCLUDE 'divrt.h'

C  Local variables
      DOUBLE PRECISION
     +     fusrat,pht,powht,sbar,sigvdt,taup,zion

C  External functions
      DOUBLE PRECISION bpol
      EXTERNAL bpol

C  External routines
      EXTERNAL beamfus,betcom,palph,palph2,pcond,phyaux,radpwr,rether,
     +     stblim,stdlim,stheat

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C+**PJK 16/01/96
      if (idhe3.eq.1) ftr = max(ftrit,1.0D-6)

C *** Calculate plasma composition

      call betcom(alphan,alphat,cfe0,dene,fdeut,ftrit,fhe3,ftr,ftritbm,
     +     idhe3,ignite,impc,impfe,impo,ralpne,rnbeam,te,zeff,abeam,
     +     afuel,aion,deni,dlamee,dlamie,dnalp,dnbeam,dnitot,dnla,
     +     dnprot,dnz,falpe,falpi,pcoef,rncne,rnone,rnfene,zeffai,zion)

      ten = te * pcoef
      tin = ti * pcoef

      q95 = q

C *** Calculate poloidal field

      bp = bpol(q,aspect,bt,kappa,triang)

C *** Total field

      btot = sqrt(bt**2 + bp**2)

C *** Poloidal beta

      betap = beta * ( btot/bp )**2

C *** Perform auxiliary power calculations

      call stheat(nout,0)

C *** Calculate fusion power

      call palph(alphan,alphat,deni,ealpha,fdeut,fhe3,ftr,ftrit,
     +     idhe3,iiter,pcoef,pi,ti,palp,pcharge,pneut,sigvdt)

C *** Calculate neutral beam slowing down effects
C+**PJK 01/04/98 If ignited, then ignore beam fusion effects

      if ((pnbeam.ne.0.0D0).and.(ignite.eq.0)) then
         call beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie,
     +        ealpha,enbeam,fdeut,ftrit,ftritbm,sigvdt,ten,tin,vol,
     +        zeffai,betanb,dnbeam2,palpnb)
      end if

C+**PJK 22/05/06 Added IFALPHAP switch to argument list of PALPH2

      call palph2(bt,bp,dene,deni,dnitot,ftr,falpe,falpi,palpnb,
     +     ifalphap,pcharge,pcoef,pneut,te,ti,vol,alpmw,betaft,
     +     palp,palpi,palpe,pfuscmw,powfmw)

C *** Neutron wall load

      if (iwalld.eq.1) then
         wallmw = ffwal * (pneut*vol) / sarea
      else
         wallmw = ffwal * (pneut*vol) / fwarea
      end if

C *** Calculate ion/electron equilibration power

      call rether(alphan,alphat,dene,dlamie,te,ti,zeffai,pie)

C *** Calculate radiation power

      call radpwr(alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95,rmajor,
     +     rminor,ralpne,rncne,rnone,rnfene,ssync,ten,vol,pbrem,plrad,
     +     prad,psync)

C *** Limit for minimum radiation power

      pht = 1.0D-6 * (pinji + pinje) + alpmw + pcharge*vol
      pbrem = max( (fradmin*pht/vol), pbrem)
      prad = pbrem + psync

C *** Heating power to plasma

      powht = alpmw + pcharge*vol + (pinje+pinji)*1.0D-6 + pohmpv*vol
     +     - prad*vol

C *** Power to divertor

      pdivt = powht - plrad*vol
      pdivt = max(0.001D0, pdivt)

C *** Calculate density limit

      call stdlim(alphan,bt,powht,rmajor,rminor,dnelimt)

C *** Calculate transport losses and energy confinement time using the
C *** chosen scaling law

      call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hfact,
     +     iinvqd,isc,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji,
     +     plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q95,qstar,vol,
     +     xarea,zeff,ptre,ptri,tauee,tauei,taueff,powerht)

C *** Calculate auxiliary physics related information
C *** for the rest of the code

      sbar = 1.0D0
      call phyaux(aspect,dene,deni,idhe3,plascur,powfmw,sbar,dnalp,
     +     dnprot,taueff,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

C *** Calculate beta limit

      call stblim(betalim)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STHEAT(nout,iprint)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.010
C
C--Description
C  Routine to calculate the auxiliary heating power
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 April 1998
C
C--Reference
C  AEA FUS 172 : Physics Assessment for the European Reactor Study
C  
C--History
C  29/06/94 PJK 1.000 Initial version
C  01/04/98 PJK 1.010 Modified call to CULNBI
C
C--Arguments
C  nout   : (INPUT)  Fortran output unit identifier
C  iprint : (INPUT)  Flag denoting whether to produce output
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'osections.h'
      INCLUDE 'stella.h'

C  Arguments
      INTEGER iprint,nout

C  Local variables
      DOUBLE PRECISION
     +     effnbss,fpion,fshine

C  External routines
      EXTERNAL culnbi,oblnkl,ocmmnt,oheadr,ovarre

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      if (iprint.eq.1) goto 10

C *** Assign heating power to the desired mechanism

      if (isthtr.eq.1) then

C *** Electron cyclotron resonance heating

         echpwr = pheat
         pinji = 0.0D0
         pinje = echpwr

      else if (isthtr.eq.2) then

C *** Lower Hybrid heating

         plhybd = pheat
         pinji = 0.0D0
         pinje = plhybd

      else if (isthtr.eq.3) then

C *** Neutral beam injection heating
C *** Use routine described in AEA FUS 172, but discard the current
C *** drive efficiency as this is irrelevant for stellarators. We are
C *** only really interested in fpion, fshine and taubeam.

         call culnbi(
     +        abeam,alphan,alphat,aspect,dene,deni,dlamie,dnla,enbeam,
     +        eps,feffcd,frbeam,ftr,ralpne,rmajor,rminor,rncne,rnfene,
     +        rnone,te,ten,zeff,zeffai,effnbss,fpion,fshine,taubeam)

         pnbeam = pheat
         pinji = pnbeam * fpion
         pinje = pnbeam * (1.0D0-fpion)

      else
         write(*,*) 'Error in routine STHEAT:'
         write(*,*) 'Illegal value for switch ISTHTR, = ',isthtr
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Calculate neutral beam current

      if (ABS(pnbeam).gt.1.0D-8) then
         cnbeam = 1.0D-3 * pnbeam / enbeam
      else
         cnbeam = 0.0D0
      end if

C *** Ratio of fusion to injection power

      if ((pinje+pinji).eq.0.0D0) then
         bigq = 1.0D18
      else
         bigq = 1.0D6*powfmw / (pinje+pinji)
      end if

C *** Output section

 10   continue

      if ((iprint.eq.0).or.(sect04.eq.0)) goto 1000

      call oheadr(nout,'Auxiliary Heating System')


      if (isthtr.eq.1) then
         call ocmmnt(nout,'Electron Cyclotron Resonance Heating')
         call oblnkl(nout)

      else if (isthtr.eq.2) then
         call ocmmnt(nout,'Lower Hybrid Heating')
         call oblnkl(nout)

      else if (isthtr.eq.3) then
         call ocmmnt(nout,'Neutral Beam Injection Heating')
         call oblnkl(nout)

      else
         WRITE(*,*) 'Error in routine STHEAT:'
         WRITE(*,*) 'Illegal value of ISTHTR, = ',isthtr
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

      call ovarre(nout,'Auxiliary power supplied to plasma (W)',
     +     '(pheat)',pheat)
      call ovarre(nout,'Energy multiplication factor Q','(bigq)',bigq)

      if (abs(pnbeam).gt.1.0D-8) then
         call ovarre(nout,'Neutral beam energy (keV)','(enbeam)',enbeam)
         call ovarre(nout,'Neutral beam current (A)','(cnbeam)',cnbeam)
         call ovarre(nout,'Fraction of beam energy to ions','(fpion)',
     +        fpion)
         call ovarre(nout,'Neutral beam shine-through','(fshine)',
     +        fshine)
         call ovarre(nout,'R injection tangent / R-major','(frbeam)',
     +        frbeam)
         call ovarre(nout,'Beam decay lengths to centre','(taubeam)',
     +        taubeam)
      end if

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STCOIL(nout,iprint)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine that performs the calculations for stellarator TF coils.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  20 July 1994
C
C--Reference
C  None
C  
C--History
C  20/07/94 PJK 1.000 Initial version
C
C--Arguments
C  nout   : (INPUT)  Fortran output unit identifier
C  iprint : (INPUT)  Flag denoting whether to produce output to file
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'build.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'fwblsh.h'

C  Arguments
      INTEGER nout,iprint

C  Local variables
      DOUBLE PRECISION awpc,awptf,leni,leno,rbcndut,rcoil,tftort
      INTEGER narc

C  External routines
      EXTERNAL outtf,stclen,stcshp,stresscl,tfcind

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Produce output to file if required

      if (iprint.eq.1) then
         call outtf(nout)
         goto 1000
      end if

C *** Cross-sectional area of outboard leg
C *** Same strange algorithm as in SCTFCOIL

      arealeg = 4.0D0 * tfthko * (rtot-tfthko/2.0D0) * sin(pi/tfno)

C *** Radius of centre of inboard leg

      rtfcin = bore + ohcth + gapoh + bcylth + tfcth/2.0D0

C *** Radius of outer edge of inboard leg

      rcoil = rtfcin + tfcth/2.0D0

C *** Unlike in a tokamak, the inboard legs do not necessarily form
C *** a continuous ring. tftort is calculated ensuring that adjacent
C *** coils are not wider than their inboard edge centres are apart.

C *** Thickness of inboard leg in toroidal direction

      tftort = min(tfcth,2.0D0*(rcoil-tfcth)*tan(pi/tfno))

C *** Total area of inboard legs (coils have rectangular cross-section)

      tfareain = tfno * tfcth * tftort

C *** Total current in TF coils

      ritfc = oacdcp * tfareain

C *** Peak toroidal field and radius of its occurrence
C *** Tokamak values; may be inaccurate for non-planar TF coils

      rbmax = rcoil
      bmaxtf = ritfc * rmu0 / (2.0D0*pi*rbmax)

C *** Peak field including ripple (arbitrarily set at 9 per cent)

      bmaxtfrp = bmaxtf * 1.09D0

C *** Centering force

      cforce = bmaxtf * ritfc / (2.0D0*tfno)

C *** Vertical force

      vforce = 0.5D0 * bt * rmajor * ritfc/2.0D0 * log(rtot/rtfcin)
     +     / tfno

C *** Horizontal and vertical bores

      tfboreh = rtot - rtfcin - 0.5D0*(tfthko+tfcth)
      borev = 2.0D0 * hmax

C *** Determine coil shape

      call stcshp

C *** Calculate TF coil self inductance

      narc = 4
      call tfcind(narc,xarc,yarc,xctfc,yctfc,dthet,tfcth,tfind)

C *** TF coil energy (0.5*L*I**2) (GJ)

      estotf = 1.0D-9 * tfind/2.0D0 / tfno * ritfc**2

C *** Calculate length of a TF coil

      call stclen(tfleng)

      tfleng = tfleng / tfno

C *** Modify case thickness if simple stress model is to be used

      if (itfmod.ne.1) thkcas = tfcth*0.5D0

C *** Winding pack dimensions
C *** A simple rectangular winding pack is assumed

C *** Radial dimension

      thkwp = tfcth - thkcas - 2.0D0*tinstf - casthi

C *** 'Toroidal' dimension

      wwp1 = tftort - 2.0D0*(casths+tinstf)

C *** Cross-sectional area

      awptf = thkwp * wwp1

C *** Cross-sectional area including insulation

      awpc = (thkwp+2.0D0*tinstf) * (wwp1+2.0D0*tinstf)

C *** Cross-sectional area of surrounding case

      acasetf = tfcth*tftort - awpc

C *** Checks for negative lengths or areas

      if ( (thkwp.le.0.0D0).or.
     +     (wwp1.le.0.0D0).or.
     +     (awptf.le.0.0D0).or.
     +     (awpc.le.0.0D0).or.
     +     (acasetf.le.0.0D0) ) then
         write(*,*) 'Error in routine STCOIL:'
         write(*,*) 'Winding pack cross-section problem'
         write(*,*) 'tftort = ',tftort
         write(*,*) 'thkwp = ',thkwp
         write(*,*) 'wwp1 = ',wwp1
         write(*,*) 'awptf = ',awptf
         write(*,*) 'awpc = ',awpc
         write(*,*) 'acasetf = ',acasetf
         write(*,*) ' '
         write(*,*) 'PROCESS continuing...'
      end if

C *** Winding pack current density

      jwptf = ritfc/(tfno*awptf)

C *** Superconducting cable information

C *** Radius of rounded corners of cable space inside conduit

      rbcndut = thwcndut * 0.75D0

C *** Dimension of square cross-section of each turn

      leno = sqrt(cpttf / jwptf)

C *** Dimension of square cable space inside insulation and case of
C *** the conduit of each turn

      leni = leno - 2.0D0 * (thwcndut + thicndut)

      if (leni.le.0.0D0) then
         write(*,*) 'Error in routine STCOIL:'
         write(*,*) 'Cable space dimension, leni = ',leni
         write(*,*) 'Reduce conduit case or insulation thicknesses,'
         write(*,*) 'or increase cpttf value or lower bound.'
         write(*,*) ' '
      end if

C *** Cross-sectional area of cable space per turn

      acstf = sign(1.0D0,leni) * ( leni**2 - (4.0D0-pi)*rbcndut**2 )

      if (acstf.le.0.0D0) then
         if (leni.lt.0.0D0) then
            write(*,*) 'Warning in routine STCOIL:'
            write(*,*) '    Cable space area, acstf = ',acstf
            write(*,*) 'Cable space dimension, leni = ',leni
            write(*,*) ' '
         else
            write(*,*) 'Warning in routine STCOIL:'
            write(*,*) '    Cable space area, acstf = ',acstf
            write(*,*) 'Cable space dimension, leni = ',leni
            write(*,*) 'Artificially set rounded corner radius to zero'
            write(*,*) ' '
            acstf = leni**2
         end if
      end if

C *** Cross-sectional area of conduit case per turn

      acndttf = (leni + 2.0D0*thwcndut)**2 - acstf

C *** Total number of turns per TF coil

      turnstf = ritfc / (cpttf * tfno)

C *** Total conductor cross-sectional area, taking account of void area

      acond = acstf * turnstf * (1.0D0 - vftf)

C *** Void area in cable, for He

      avwp = acstf * turnstf * vftf

C *** Insulation area (not including ground-wall)

      aiwp = turnstf * (leno**2 - acndttf - acstf)

C *** Structure area for cable

      aswp = (turnstf*acndttf)

C *** Half-width of side of coil nearest torus centreline

      tfocrn = 0.5D0 * tftort

C *** Half-width of side of coil nearest plasma

      tficrn = 0.5D0 * tftort

C *** Total surface area of coil side facing plasma: inboard region

      tfsai = 4.0D0 * tfno * tficrn * hr1

C *** Total surface area of coil side facing plasma: outboard region

      tfsao = 2.0D0 * tfno * tficrn * (tfleng - 2.0D0*hr1)

C *** Mass of case. Correction factor included to maintain enough
C *** structure when the case is a small fraction of the total coil

      whtcas = casfact * tfleng * acasetf * dcase *
     +     sqrt(0.41D0 * tfareain/(acasetf*tfno))

C *** Masses of conductor constituents

C *** Superconductor

      whtconsc = tfleng * turnstf * acstf*(1.0D0-vftf) *
     +     (1.0D0-fcutfsu)*dcond(isumattf)

C *** Copper

      whtconcu = tfleng * turnstf * acstf*(1.0D0-vftf) *
     +     fcutfsu*dcopper

C *** Steel conduit (sheath)

      whtconsh = tfleng * turnstf * acndttf * denstl

C *** Total conductor mass

      whtcon = whtconsc + whtconcu + whtconsh

C *** Bucking cylinder mass (assumed not to exist)

      wtbc = 0.0D0

C *** Total TF coil mass

      whttf = (whtcas+whtcon) * tfno

C *** Perform stress calculations

      call stresscl

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STCSHP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine that approximates the shape of the stellarator TF coils
C  using 4 circular arcs along the edge facing the plasma.
C  The coils are assumed to be elliptical in cross-section, with an
C  elongation ymax/xmax of 1.1.
C  
C  This is clearly a highly dodgy approximation...
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  20 July 1994
C
C--Reference
C  The code is almost identical to that in routine COILSHAP.
C  
C--History
C  20/07/94 PJK 1.000 Initial version
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
      INCLUDE 'build.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'

C  Local variables
      DOUBLE PRECISION acoil, thet2, thet3, thet4

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Mean coil minor radius

      acoil = 0.5D0 * ((rtot-tfthko/2.0D0) - (rtfcin+tfcth/2.0D0))

C *** Point on inboard midplane

      xarc(1) = rtfcin + tfcth/2.0D0
      yarc(1) = 0.0D0

C *** Point at top of coil

      xarc(3) = (rtfcin+tfcth/2.0D0) + acoil
      yarc(3) = hmax*1.1D0

C *** Point on outboard midplane

      xarc(5) = rtot - tfthko/2.0D0
      yarc(5) = 0.0D0

C *** Point on inboard edge

      xarc(2) = xarc(3) - sqrt(0.5555555555D0*acoil*acoil)
      yarc(2) = 0.6666666666D0 * yarc(3)

C *** Point on outboard edge

      xarc(4) = xarc(3) + sqrt(0.5555555555D0*acoil*acoil)
      yarc(4) = 0.6666666666D0 * yarc(3)

C *** Find arc centres

      yctfc(4) = 0.d0
      xctfc(4) = (xarc(5)**2 - xarc(4)**2 - yarc(4)**2)/2.d0
     +     /(xarc(5) - xarc(4))
      thet4 = atan2(yarc(4) , (xarc(4)-xctfc(4)) )
      dthet(4) = abs(thet4)
      radctf(4) = sqrt( (yarc(4)-yctfc(4))**2 + (xarc(4)-xctfc(4))**2)

      xctfc(3) =(2.d0*(yarc(4) - yarc(3))* (yarc(4)-tan(thet4)*xarc(4))
     +     + xarc(3)**2 + yarc(3)**2 - xarc(4)**2 - yarc(4)**2 ) /
     +     2.d0/( (xarc(3) -xarc(4)) - (yarc(4) - yarc(3))*tan(thet4) )
      yctfc(3) = yarc(4) - tan(thet4) * (xarc(4) - xctfc(3))
      thet3 = atan2( (yarc(3)-yctfc(3)), (xarc(3) - xctfc(3)))
      dthet(3) = abs(thet3 - thet4)
      radctf(3) = sqrt( (yarc(3)-yctfc(3))**2 + (xarc(3)-xctfc(3))**2)

      xctfc(2) =(2.d0*(yarc(3) - yarc(2))*(yarc(3)-tan(thet3)*xarc(3))
     +     + xarc(2)**2 + yarc(2)**2 - xarc(3)**2 - yarc(3)**2) /
     +     2.d0/ ( (xarc(2) - xarc(3)) - (yarc(3) - yarc(2))*tan(thet3))
      yctfc(2) = yarc(3) - tan(thet3) * (xarc(3) - xctfc(2))
      thet2 = atan2( (yarc(2) - yctfc(2)), (xarc(2) - xctfc(2)) )
      dthet(2) = abs(abs(thet2) - thet3)
      radctf(2) = sqrt( (yarc(2)-yctfc(2))**2 + (xarc(2)-xctfc(2))**2 )

      xctfc(1) = ( xarc(2)**2 - xarc(1)**2 + yarc(2)**2)/
     +     (2.d0*(xarc(2)-xarc(1)))
      yctfc(1) = 0.d0
      radctf(1) = xctfc(1) - xarc(1)
      dthet(1) = atan2(yarc(2), (xctfc(1)-xarc(1)))

C *** Half-height of TF coil inboard leg 'straight section'

      hr1 = yarc(2)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STCLEN(totlen)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the total length of the coils in a stellarator.
C  The coil geometry is based on that of the Wendelstein VII-X design,
C  with five different coils being repeated ten times to produce the
C  complete 50-coil set.
C  The data are scaled with the machine's average major radius and
C  the mean coil minor radius.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  11 July 1994
C
C--Reference
C  Wendelstein VII-X: Application for Preferential Support, Aug 1990
C  
C--History
C  11/07/94 PJK 1.000 Initial version
C
C--Arguments
C  totlen : (OUTPUT) Total coil length (m)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'

C  Arguments
      DOUBLE PRECISION totlen

C  Local variables
      DOUBLE PRECISION a0,a0i,a0o,dlen,phi(58,5),pp,rp,theta(58),
     +     x0,x1,x2,y0,y1,y2,z0,z1,z2,zp
      INTEGER icoil,itheta

C  External routines
      EXTERNAL rzp,rzpxyz,stcdat

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Coil mean minor radius (m) --- average of inboard and outboard

      a0i = 0.5D0*tfcth + ddwi + gapds + shldith + blnkith + fwith +
     +     scrapli + rminor

      a0o = rminor + scraplo + fwoth + blnkoth + shldoth + gapsto +
     +     ddwi + 0.5D0*tfthko

      a0 = 0.5D0*(a0i + a0o)

C *** Read in coil geometry as a function of (theta,phi)

      call stcdat(theta,phi)

      totlen = 0.0D0

      do 20 icoil = 1,5

C *** Find (x,y,z) of first point on coil

         call rzp(phi(1,icoil),theta(1),rmajor,a0,rp,zp,pp)
         call rzpxyz(rp,zp,pp,x0,y0,z0)

         x1 = x0
         y1 = y0
         z1 = z0

         do 10 itheta = 2,58

C *** Convert from (phi,theta) to (R,Z,phi)

            call rzp(phi(itheta,icoil),theta(itheta),rmajor,a0,rp,zp,pp)

C *** Convert from (R,Z,phi) to (x,y,z)

            call rzpxyz(rp,zp,pp,x2,y2,z2)

            dlen = sqrt((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)

            totlen = totlen + dlen

            x1 = x2
            y1 = y2
            z1 = z2

 10      continue

C *** Add distance between first and last points on coil

         dlen = sqrt((x0-x1)**2 + (y0-y1)**2 + (z0-z1)**2)
         totlen = totlen + dlen

 20   continue

C *** Multiply by ten to obtain total length for whole 50-coil set

      totlen = totlen*10.0D0

      end
C----------------------------------------------------------------------
      SUBROUTINE RZP(phi,theta,r0,a0,rp,zp,pp)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to convert the (phi,theta) coordinates defining the position
C  of a point on the stellarator coils to (R,Z,phi) coordinates, given
C  the average major and minor radii.
C  
C  The (phi,theta) data are taken from the Wendelstein VII-X geometry,
C  as is the locus of the minor axis. However, the (R,Z) cross-section
C  of the surface on which the coils lie is complicated in W VII-X,
C  so this is approximated here by an ellipse whose major axis
C  rotates continuously with toroidal angle.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  12 July 1994
C
C--Reference
C  Wendelstein VII-X: Application for Preferential Support, Aug 1990
C  
C--History
C  12/07/94 PJK 1.000 Initial version
C
C--Arguments
C  phi    : (INPUT)  Toroidal angle of point on coil (rad)
C  theta  : (INPUT)  Poloidal angle of point on coil (rad)
C  r0     : (INPUT)  Coil mean major radius (m)
C  a0     : (INPUT)  Coil mean minor radius (m)
C  rp     : (OUTPUT) Major radius of point (m)
C  zp     : (OUTPUT) Vertical coordinate of point (m)
C  pp     : (OUTPUT) Toroidal angle of point (rad)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION pio2
      PARAMETER (pio2 = 1.5707963267948966D0)

C  Arguments
      DOUBLE PRECISION a0,phi,pp,r0,rp,theta,zp

C  Local variables
      DOUBLE PRECISION a,da,rma,zma

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** (R,Z) of minor axis

      rma = r0
     +     + 0.0775D0*r0*cos(5.0D0*phi)
     +     - 1.8537D-3*r0*cos(10.0D0*phi)
     +     + 3.0595D-4*r0*cos(17.0D0*phi)

      zma = 0.0632D0*r0*sin(5.0D0*phi)
     +     + 1.2058D-3*r0*sin(10.0D0*phi)

C *** Approximation of (R,Z) cross-section of surface on which coil
C *** conductors lie. This describes an ellipse that rotates with
C *** toroidal angle.

      da = 0.1D0 * a0

      a = a0 + da*sin(2.0D0*theta + 5.0D0*phi - pio2)

C *** Corrected (R,Z,phi) coordinates

      rp = rma + a*cos(theta)
      zp = zma + a*sin(theta)
      pp = phi

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RZPXYZ(rp,zp,pp,x,y,z)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to convert from (R,Z,phi) coordinates to Cartesian (x,y,z)
C  coordinates. (R,0,0) corresponds to the x-axis.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  12 July 1994
C
C--Reference
C  None
C  
C--History
C  12/07/94 PJK 1.000 Initial version
C
C--Arguments
C  rp     : (INPUT)  Radial coordinate (m)
C  zp     : (INPUT)  Vertical coordinate (m)
C  pp     : (INPUT)  Toroidal angle coordinate (rad)
C  x      : (OUTPUT) Cartesian X coordinate (m)
C  y      : (OUTPUT) Cartesian Y coordinate (m)
C  z      : (OUTPUT) Cartesian Z coordinate (m)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION rp,zp,pp,x,y,z

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      x = rp*cos(pp)
      y = rp*sin(pp)
      z = zp

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STCDAT(theta,phi)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to load stored coil geometry data into arrays theta and phi.
C  Data measured from Wendelstein VII-X coil set.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  11 July 1994
C
C--Reference
C  Figure 25: Part 1, section 6: Wendelstein VII-X, Application for
C  Preferential Support, August 1990
C  
C--History
C  11/07/94 PJK 1.000 Initial version
C
C--Arguments
C  theta  : (OUTPUT) Array containing poloidal angle values for points
C                    on the stellarator coils
C  phi    : (OUTPUT) Array containing toroidal angle values for points
C                    on the stellarator coils
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION twopi
      PARAMETER (twopi = 6.28318530717958648D0)

C  Arguments
      DOUBLE PRECISION theta(58),phi(58,5)

C  Local variables
      DOUBLE PRECISION philoc(58,5)
      INTEGER icoil,itheta

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Toroidal angles as a function of theta, around each coil

      DATA (philoc(itheta,1),itheta=1,58)/
     +     5.79986D-02, 5.79986D-02, 5.79986D-02, 5.31654D-02,
     +     5.31654D-02, 4.83322D-02, 4.83322D-02, 4.34990D-02,
     +     3.38325D-02, 2.41661D-02, 1.44997D-02, 4.83322D-03,
     +     0.00000D+00,-1.44997D-02,-2.41661D-02,-2.41661D-02,
     +    -2.41661D-02,-2.41661D-02,-1.93329D-02,-1.93329D-02,
     +    -2.89993D-02,-3.38325D-02,-4.83322D-02,-5.31654D-02,
     +    -4.34990D-02,-2.89993D-02,-9.66644D-03, 9.66644D-03,
     +     2.89993D-02, 4.83322D-02, 6.76651D-02, 8.21647D-02,
     +     1.01498D-01, 1.15997D-01, 1.35330D-01, 1.49830D-01,
     +     1.59496D-01, 1.59496D-01, 1.44997D-01, 1.35330D-01,
     +     1.30497D-01, 1.25664D-01, 1.25664D-01, 1.25664D-01,
     +     1.25664D-01, 1.25664D-01, 1.15997D-01, 1.11164D-01,
     +     1.06331D-01, 1.01498D-01, 9.66644D-02, 8.69980D-02,
     +     7.73315D-02, 7.73315D-02, 7.24983D-02, 6.76651D-02,
     +     6.76651D-02, 6.28319D-02/

      DATA (philoc(itheta,2),itheta=1,58)/
     +     1.93329D-01, 1.88496D-01, 1.83662D-01, 1.83662D-01,
     +     1.78829D-01, 1.73996D-01, 1.69163D-01, 1.64329D-01,
     +     1.64329D-01, 1.59496D-01, 1.54663D-01, 1.49830D-01,
     +     1.44997D-01, 1.40163D-01, 1.35330D-01, 1.25664D-01,
     +     1.15997D-01, 1.06331D-01, 1.01498D-01, 9.66644D-02,
     +     9.66644D-02, 9.18312D-02, 9.18312D-02, 8.69980D-02,
     +     7.24983D-02, 6.28319D-02, 5.79986D-02, 6.28319D-02,
     +     7.73315D-02, 9.66644D-02, 1.15997D-01, 1.35330D-01,
     +     1.49830D-01, 1.64329D-01, 1.78829D-01, 1.93329D-01,
     +     2.12662D-01, 2.31995D-01, 2.46494D-01, 2.56161D-01,
     +     2.60994D-01, 2.51327D-01, 2.36828D-01, 2.27161D-01,
     +     2.17495D-01, 2.12662D-01, 2.12662D-01, 2.07828D-01,
     +     2.07828D-01, 2.07828D-01, 2.07828D-01, 2.07828D-01,
     +     2.07828D-01, 2.07828D-01, 2.02995D-01, 2.02995D-01,
     +     1.98162D-01, 1.98162D-01/

      DATA (philoc(itheta,3),itheta=1,58)/
     +     3.18992D-01, 3.23826D-01, 3.28659D-01, 3.33492D-01,
     +     3.33492D-01, 3.33492D-01, 3.28659D-01, 3.23826D-01,
     +     3.18992D-01, 3.09326D-01, 2.99660D-01, 2.94826D-01,
     +     2.89993D-01, 2.89993D-01, 2.85160D-01, 2.80327D-01,
     +     2.80327D-01, 2.75494D-01, 2.70660D-01, 2.60994D-01,
     +     2.51327D-01, 2.41661D-01, 2.31995D-01, 2.17495D-01,
     +     2.07828D-01, 1.98162D-01, 1.83662D-01, 1.64329D-01,
     +     1.59496D-01, 1.59496D-01, 1.64329D-01, 1.78829D-01,
     +     1.93329D-01, 2.12662D-01, 2.27161D-01, 2.41661D-01,
     +     2.56161D-01, 2.70660D-01, 2.80327D-01, 3.04493D-01,
     +     3.14159D-01, 3.23826D-01, 3.38325D-01, 3.47992D-01,
     +     3.67325D-01, 3.86658D-01, 3.86658D-01, 3.57658D-01,
     +     3.18992D-01, 2.99660D-01, 2.89993D-01, 2.94826D-01,
     +     2.94826D-01, 2.99660D-01, 3.09326D-01, 3.14159D-01,
     +     3.18992D-01, 3.18992D-01/

      DATA (philoc(itheta,4),itheta=1,58)/
     +     4.15657D-01, 4.25323D-01, 4.34990D-01, 4.44656D-01,
     +     4.54323D-01, 4.63989D-01, 4.68822D-01, 4.73656D-01,
     +     4.73656D-01, 4.68822D-01, 4.63989D-01, 4.54323D-01,
     +     4.49489D-01, 4.44656D-01, 4.39823D-01, 4.39823D-01,
     +     4.34990D-01, 4.34990D-01, 4.34990D-01, 4.30157D-01,
     +     4.25323D-01, 4.15657D-01, 4.01157D-01, 3.86658D-01,
     +     3.67325D-01, 3.47992D-01, 3.38325D-01, 3.23826D-01,
     +     3.18992D-01, 3.09326D-01, 2.94826D-01, 2.85160D-01,
     +     2.75494D-01, 2.75494D-01, 2.80327D-01, 2.89993D-01,
     +     2.99660D-01, 3.14159D-01, 3.28659D-01, 3.43159D-01,
     +     3.52825D-01, 3.67325D-01, 3.81824D-01, 3.91491D-01,
     +     4.01157D-01, 4.15657D-01, 4.34990D-01, 4.54323D-01,
     +     4.73656D-01, 4.88155D-01, 4.97822D-01, 4.83322D-01,
     +     4.10824D-01, 3.91491D-01, 3.91491D-01, 3.96324D-01,
     +     4.05990D-01, 4.15657D-01/

      DATA (philoc(itheta,5),itheta=1,58)/
     +     5.02655D-01, 5.07488D-01, 5.17154D-01, 5.31654D-01,
     +     5.41321D-01, 5.55820D-01, 5.70320D-01, 5.89653D-01,
     +     6.04152D-01, 6.13819D-01, 6.23485D-01, 6.28319D-01,
     +     6.28319D-01, 6.23485D-01, 6.18652D-01, 6.13819D-01,
     +     6.08986D-01, 6.04152D-01, 5.99319D-01, 5.99319D-01,
     +     5.94486D-01, 5.89653D-01, 5.84820D-01, 5.79986D-01,
     +     5.70320D-01, 5.55820D-01, 5.36487D-01, 5.21988D-01,
     +     5.02655D-01, 4.83322D-01, 4.73656D-01, 4.68822D-01,
     +     4.63989D-01, 4.63989D-01, 4.59156D-01, 4.54323D-01,
     +     4.49489D-01, 4.34990D-01, 4.25323D-01, 4.20490D-01,
     +     4.20490D-01, 4.25323D-01, 4.34990D-01, 4.39823D-01,
     +     4.54323D-01, 4.73656D-01, 4.92988D-01, 5.12321D-01,
     +     5.31654D-01, 5.46154D-01, 5.65487D-01, 5.79986D-01,
     +     5.89653D-01, 5.94486D-01, 5.89653D-01, 5.60653D-01,
     +     5.21988D-01, 5.12321D-01/

C *** Calculate poloidal angles

      do 10 itheta = 1,58
         theta(itheta) = dble(itheta)*twopi/58.0D0
 10   continue

C *** Store toroidal angles

      do 30 icoil = 1,5
         do 20 itheta = 1,58
            phi(itheta,icoil) = philoc(itheta,icoil)
 20      continue
 30   continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STFWBS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.010
C
C--Description
C  Routine to calculate first wall, blanket and shield properties not
C  already calculated in FWBS.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  10 June 1996
C
C--Reference
C  None
C  
C--History
C  01/07/94 PJK 1.000 Initial version
C  10/06/96 PJK 1.010 Moved first wall area calculation into STBILD
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'cost.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'divrt.h'
      INCLUDE 'osections.h'
      INCLUDE 'blanket.h'

C  Arguments

C  Global variables

C  Local variables

C  External functions

C  External routines

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STDLIM(alphan,bt,powht,rmajor,rminor,dlimit)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the density limit in a stellarator.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  30 June 1994
C
C--Reference
C  S.Sudo, Y.Takeiri, H.Zushi et al., "Scalings of Energy Confinement
C  and Density Limit in Stellarator/Heliotron Devices", Nuclear Fusion
C  vol.30, 11 (1990).
C  
C--History
C  30/06/94 PJK 1.000 Initial version
C
C--Arguments
C  alphan : (INPUT)  Density profile index
C  bt     : (INPUT)  Toroidal field on axis (T)
C  powht  : (INPUT)  Absorbed heating power (MW)
C  rmajor : (INPUT)  Plasma major radius (m)
C  rminor : (INPUT)  Plasma minor radius (m)
C  dlimit : (OUTPUT) Maximum volume-averaged plasma density (/m3)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION alphan,bt,powht,rmajor,rminor,dlimit

C  Local variables
      DOUBLE PRECISION arg,denom,dnlamx

C  External functions
      DOUBLE PRECISION gamfun
      EXTERNAL gamfun

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      arg = powht*bt / (rmajor*rminor*rminor)

      if (arg.le.0.0D0) then
         write(*,*) 'Error in routine STDLIM:'
         write(*,*) 'NaN result (sqrt) will occur, arg = ',arg
         write(*,*) ' powht = ',powht
         write(*,*) '    bt = ',bt
         write(*,*) 'rmajor = ',rmajor
         write(*,*) 'rminor = ',rminor
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Maximum line-averaged electron density

      dnlamx = 0.25D20 * sqrt(arg)

C *** Scale the result so that it applies to the volume-averaged
C *** electron density

      denom = 0.886227D0 * gamfun(alphan+1.0D0) * (1.0D0+alphan)

      if (denom.eq.0.0D0) then
         write(*,*) 'Error in routine STDLIM:'
         write(*,*) 'Divison by zero will occur, denom = ',denom
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

      dlimit = dnlamx * gamfun(alphan+1.5D0) / denom

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STBLIM(betamx)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the beta limit in a stellarator
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  30 June 1994
C
C--Reference
C  J.F.Lyon, K.Gulec, R.L.Miller and L.El-Guebaly, "Status of the U.S.
C  Stellarator Reactor Study"
C  
C--History
C  30/06/94 PJK 1.000 Initial version
C
C--Arguments
C  betamx : (OUTPUT)  Maximum volume-averaged plasma beta
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION betamx

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      betamx = 0.05D0

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STIGMA(nout)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.040
C
C--Description
C  Routine to calculate ignition margin at the final point
C  with different stellarator confinement time scaling laws
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  16 July 2001
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  30/06/94 PJK 1.000 Initial version
C  01/04/98 PJK 1.010 Modified call to PCOND
C  30/06/98 PJK 1.020 Modified call to PCOND
C  19/01/99 PJK 1.030 Modified call to PCOND
C  16/07/01 PJK 1.040 Modified call to PCOND
C
C--Arguments
C  nout   : (INPUT)  Fortran unit specifier for main output
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'labels.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER nout

C  Local variables
      DOUBLE PRECISION d2,powerhtz,ptrez,ptriz,taueez,taueezz,
     +     taueffz,taueiz
      INTEGER i,iisc,nstlaw,istlaw(10)

C  External functions
      DOUBLE PRECISION fhfac
      EXTERNAL         fhfac

C  External routines
      EXTERNAL oblnkl,osubhd,pcond

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Only produce output if required

      if (sect03.eq.0) goto 1000

C *** Start output

      call osubhd(nout,'Confinement times, and required H-factors :')

      write(nout,10)
 10   format(
     +     t5,'scaling law',
     +     t30,'confinement time (s)',
     +     t55,'H-factor for')

      write(nout,20)
 20   format(
     +     t34,'for H = 2',
     +     t54,'power balance')

      call oblnkl(nout)

C *** Label stellarator scaling laws (update if more are added)

      nstlaw = 3
      istlaw(1) = 21
      istlaw(2) = 22
      istlaw(3) = 23

C *** Calculate power balances for all stellarator scaling laws
C *** assuming H = 2

      do 40 iisc = 1,nstlaw
         i = istlaw(iisc)
         d2 = 2.0D0

         call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,d2,
     +        iinvqd,i,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji,
     +        plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q,qstar,vol,
     +        xarea,zeff,ptrez,ptriz,taueez,taueiz,taueffz,powerhtz)

         hfac(iisc) = fhfac(i)

         write(nout,30) tauscl(istlaw(iisc)),taueez,hfac(iisc)
 30      format(t2,a24,t34,f7.3,t58,f7.3)

 40   continue

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STOUT(nout)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.300
C
C--Description
C  Routine to print out the final machine parameters
C
C--Author
C  Peter Knight D3/162a Culham Science Centre, ext.4181
C
C--Date
C  19 May 1999
C
C--Reference
C  None
C  
C--History
C  30/06/94 PJK 1.000 Initial version
C  27/02/96 PJK 1.010 Added use of IFISPACT
C  23/01/97 PJK 1.100 Split routine POWER into POWER1 and POWER2
C  26/02/97 PJK 1.200 Added routine LOCA
C  18/11/97 PJK 1.210 Removed NOUT argument from FISPAC call
C  19/05/99 PJK 1.300 Added call to routine AVAIL
C
C--Arguments
C  nout   : (INPUT)  Fortran output unit identifier
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'

C  Arguments
      INTEGER nout

C  External routines
      EXTERNAL acpow,avail,bldgcall,costs,divcall,fispac,fwbs,loca,
     +     outplas,power2,stbild,stheat,stigma,ststrc,stcoil,tfpwr,
     +     tfspcall,vaccall

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      call costs(nout,1)

      call avail(nout,1)

      call outplas(nout)

      call stigma(nout)

      call stheat(nout,1)

      call divcall(1,nout)

      call stbild(nout,1)

      call stcoil(nout,1)

      call tfspcall(nout,1)

      call ststrc(nout,1)

      call fwbs(nout,1)

      if (ifispact.eq.1) then
         call fispac(0)
         call fispac(1)
         call loca(nout,0)
         call loca(nout,1)
      end if

      call tfpwr(nout,1)

C+**PJK 23/01/97      call power(nout,1)

      call vaccall(nout,1)

      call bldgcall(nout,1)

      call acpow(nout,1)

C+**PJK 23/01/97
      call power2(nout,1)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE STSTRC(nout,iprint)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.010
C
C--Description
C  Routine to call the structure module.
C  This is the stellarator version of routine STRUCALL.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 February 1996
C
C--Reference
C  None
C  
C--History
C  01/07/94 PJK 1.000 Initial version
C  01/02/96 PJK 1.010 Added itfsup, ipfres to argument list of STRUCT
C
C--Arguments
C  nout   : (INPUT)  Fortran output unit identifier
C  iprint : (INPUT)  Switch denoting whether to produce output
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'build.h'
      INCLUDE 'divrt.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'phydat.h'
      INCLUDE 'struccom.h'
      INCLUDE 'tfcoil.h'

C  Arguments
      INTEGER iprint,nout

C  Local variables
      DOUBLE PRECISION ak,cnorm,twhtpf

C  External routines
      EXTERNAL struct

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Total weight of the PF coil conductor and its structure

      twhtpf = 0.0D0

C *** The intercoil support structure between the TF coils is
C *** proportional to the plasma current normalised to 22 MA.
C *** Since in stellarators, the plasma current is zero but we still
C *** need intercoil support, we will normalise the plasma current
C *** factor in the calculation to unity.

      cnorm = 2.2D7

C *** Unfortunately, setting cnorm to be non-zero causes the PF coil
C *** fence mass to be finite. To fiddle this to be zero, I will set
C *** the kappa argument to zero. (fence mass proportional to kappa)

      ak = 0.0D0

      call struct(cnorm,rmajor,rminor,ak,bt,itfsup,ipfres,tfboreh,
     +     hmax,whtshld,divmas,twhtpf,whttf,fwmass,whtblkt,coolmass,
     +     wtbc,dewmkg,nout,iprint,fncmass,aintmass,clgsmass,
     +     coldmass,gsmass)

      return
      end
