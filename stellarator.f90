!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module stellarator_module

  !+ad_name  stellarator_module
  !+ad_summ  Module containing stellarator routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  stcall
  !+ad_cont  stinit
  !+ad_cont  stgeom
  !+ad_cont  stbild
  !+ad_cont  stphys
  !+ad_cont  stheat
  !+ad_cont  stcoil
  !+ad_cont  stcshp
  !+ad_cont  stclen
  !+ad_cont  stfwbs
  !+ad_cont  stdlim
  !+ad_cont  stblim
  !+ad_cont  stigma
  !+ad_cont  stout
  !+ad_cont  ststrc
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of the first wall, blanket and shield components
  !+ad_desc  of a fusion power plant.
  !+ad_prob  None
  !+ad_call  availability_module
  !+ad_call  build_variables
  !+ad_call  buildings_module
  !+ad_call  constants
  !+ad_call  costs_module
  !+ad_call  current_drive_module
  !+ad_call  current_drive_variables
  !+ad_call  divertor_module
  !+ad_call  divertor_variables
  !+ad_call  fwbs_module
  !+ad_call  fwbs_variables
  !+ad_call  global_variables
  !+ad_call  maths_library
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_module
  !+ad_call  physics_variables
  !+ad_call  plasma_geometry_module
  !+ad_call  power_module
  !+ad_call  process_output
  !+ad_call  rfp_variables
  !+ad_call  sctfcoil_module
  !+ad_call  stellarator_variables
  !+ad_call  structure_module
  !+ad_call  structure_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  vacuum_module
  !+ad_hist  31/10/12 PJK Initial version of module
  !+ad_hist  06/11/12 PJK Added availability_module
  !+ad_hist  06/11/12 PJK Added plasma_geometry_module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use availability_module
  use build_variables
  use buildings_module
  use constants
  use costs_module
  use current_drive_module
  use current_drive_variables
  use divertor_module
  use divertor_variables
  use fwbs_module
  use fwbs_variables
  use global_variables
  use maths_library
  use numerics
  use pfcoil_variables
  use physics_module
  use physics_variables
  use plasma_geometry_module
  use power_module
  use process_output
  use rfp_variables
  use sctfcoil_module
  use stellarator_variables
  use structure_module
  use structure_variables
  use tfcoil_variables
  use times_variables
  use vacuum_module

  implicit none

  private
  public :: stcall, stinit, stout

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stcall

    !+ad_name  stcall
    !+ad_summ  Routine to call the physics and engineering modules
    !+ad_summ  relevant to stellarators
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine is the caller for the stellarator models.
    !+ad_prob  None
    !+ad_call  acpow
    !+ad_call  avail
    !+ad_call  bldgcall
    !+ad_call  costs
    !+ad_call  divcall
    !+ad_call  fwbs
    !+ad_call  geomty
    !+ad_call  power1
    !+ad_call  power2
    !+ad_call  stbild
    !+ad_call  stcoil
    !+ad_call  stfwbs
    !+ad_call  stgeom
    !+ad_call  stphys
    !+ad_call  ststrc
    !+ad_call  tfpwr
    !+ad_call  vaccall
    !+ad_hist  28/06/94 PJK Initial version
    !+ad_hist  23/01/97 PJK Split routine POWER into POWER1 and POWER2
    !+ad_hist  19/11/97 PJK Corrected call to STCOIL (missing arguments)
    !+ad_hist  19/05/99 PJK Added call to routine AVAIL
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added costs_module
    !+ad_hist  17/10/12 PJK Added divertor_module
    !+ad_hist  18/10/12 PJK Added fwbs_module
    !+ad_hist  18/10/12 PJK Added vacuum_module
    !+ad_hist  30/10/12 PJK Added power_module
    !+ad_hist  30/10/12 PJK Added buildings_module
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call stgeom
    call geomty
    call stbild(nout,0)
    call stphys
    call stcoil(nout,0)
    call ststrc(nout,0)
    call stfwbs
    call fwbs(nout,0)
    call divcall(nout,0)
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

    !+ad_name  stinit
    !+ad_summ  Routine to initialise the variables relevant to stellarators
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine initialises the variables relevant to stellarators.
    !+ad_desc  Many of these may override the values set in routine
    !+ad_desc  <A HREF="initial.html">initial</A>.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  28/06/94 PJK Initial version
    !+ad_hist  09/09/94 PJK Changed ICASE
    !+ad_hist  07/12/94 PJK Changed default q and kappa values
    !+ad_hist  04/12/95 PJK Ensured stellarators do not use D-He3 reaction
    !+ad_hist  26/02/96 PJK Modified initial setting of ISTELL (moved to
    !+ad_hisc               routine DEVTYP)
    !+ad_hist  22/01/97 PJK Subsumed heattr.h, heatrinp.h and pfelect.h into
    !+ad_hisc               htpwr.h
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  10/10/12 PJK Modified to use new numerics module
    !+ad_hist  15/10/12 PJK Added global_variables module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  31/10/12 PJK Added stellarator_variables
    !+ad_hist  23/01/13 PJK Turned off some output sections
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (istell == 0) return

    !  Numerics quantities

    boundl(1) = 5.0D0

    boundu(1) = 20.0D0
    boundu(3) = 30.0D0
    boundu(29) = 20.0D0

    icase = 'PROCESS stellarator model'

    !  Build quantities

    ohcth = 0.0D0
    iohcl = 0
    ohhghf = 0.0D0
    gapoh = 0.0D0
    bcylth = 0.0D0
    tfootfi = 1.0D0

    !  Physics quantities

    aspect = 12.5D0
    dnbeta = 0.0D0
    rmajor = 20.0D0
    kappa = 2.0D0
    triang = 0.0D0
    q = 1.03D0
    idhe3 = 0

    !  Turn off current drive

    irfcd = 0

    !  Times for different phases

    tburn = 3.15576D7  !  one year
    tohs = 0.0D0
    tpulse = 3.15576D7  !  one year
    tqnch = 0.0D0
    tramp = 0.0D0

    !  TF coil quantities

    tfno = 50.0D0

    !  Output sections

    sect05 = 0
    sect08 = 0
    sect09 = 0
    sect11 = 0
    sect21 = 0

  end subroutine stinit

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stgeom

    !+ad_name  stgeom
    !+ad_summ  Routine to calculate the plasma volume and surface area for
    !+ad_summ  a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the plasma volume and surface area for
    !+ad_summ  a stellarator configuration.
    !+ad_desc  <P>In practice, this is actually done by routine
    !+ad_desc  <A HREF="geomty.html">geomty</A>, but this routine exists as
    !+ad_desc  a placeholder for a possible future full calculation.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  28/06/94 PJK Initial version
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_stat  Currently unused
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  end subroutine stgeom

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stbild(outfile,iprint)

    !+ad_name  stbild
    !+ad_summ  Routine to determine the build of a stellarator machine
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine determines the build of the stellarator machine.
    !+ad_desc  The values calculated are based on the mean minor radius, etc.,
    !+ad_desc  as the actual radial and vertical build thicknesses vary with
    !+ad_desc  toroidal angle.
    !+ad_prob  None
    !+ad_call  obuild
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_hist  29/06/94 PJK Initial version
    !+ad_hist  10/06/96 PJK Added first wall area calculation
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  15/05/13 PJK Swapped build order of vacuum vessel and gap
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: drbild,radius

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (iprint /= 1) then

       !  Radial build to centre of plasma (should be equal to rmajor)

       rbld = bore + ohcth + gapoh + bcylth + tfcth + gapds + &
            ddwi + shldith + blnkith + fwith + scrapli + rminor

       !  Radius to inner edge of inboard shield

       rsldi = rmajor - rminor - scrapli - fwith - blnkith - shldith

       !  Radius to outer edge of outboard shield

       rsldo = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth

       !  Thickness of outboard TF coil legs

       tfthko = tfootfi*tfcth 

       !  Radius to centre of outboard TF coil legs

       gapsto = gapomin
       rtot = rsldo + ddwi + gapsto + 0.5D0*tfthko

       !  Height to inside edge of TF coil
       !  Roughly equal to average of (inboard build from TF coil to plasma
       !  centre) and (outboard build from plasma centre to TF coil)

       hmax = 0.5D0 * ( &
            (gapds+ddwi+shldith+blnkith+fwith+scrapli+rminor) + &
            (rminor+scraplo+fwoth+blnkoth+shldoth+ddwi+gapsto) )

       !  Outer divertor strike point radius, set equal to major radius

       rstrko = rmajor

       !  First wall area

       fwarea = 4.0D0*pi**2*sf*rmajor*(rminor+(scrapli+scraplo)/2.0D0) &
            * 0.875D0

    end if

    if ((iprint == 0).or.(sect06 == 0)) return

    !  Print out device build

    call oheadr(outfile,'Radial Build')

    write(outfile,10)
10  format(t43,'Thickness (m)',t60,'Radius (m)')

    radius = 0.0D0
    call obuild(outfile,'Device centreline',0.0D0,radius)

    drbild = bore + ohcth + gapoh + bcylth
    radius = radius + drbild
    call obuild(outfile,'Machine bore',drbild,radius)

    radius = radius + tfcth
    call obuild(outfile,'TF coil inboard leg',tfcth,radius)

    radius = radius + gapds
    call obuild(outfile,'Gap',gapds,radius)

    radius = radius + ddwi
    call obuild(outfile,'Vacuum vessel',ddwi,radius)

    radius = radius + shldith
    call obuild(outfile,'Inboard shield',shldith,radius)

    radius = radius + blnkith
    call obuild(outfile,'Inboard blanket',blnkith,radius)

    radius = radius + fwith
    call obuild(outfile,'Inboard first wall',fwith,radius)

    radius = radius + scrapli
    call obuild(outfile,'Inboard scrape-off',scrapli,radius)

    radius = radius + rminor
    call obuild(outfile,'Plasma geometric centre',rminor,radius)

    radius = radius + rminor
    call obuild(outfile,'Plasma outboard edge',rminor,radius)

    radius = radius + scraplo
    call obuild(outfile,'Outboard scrape-off',scraplo,radius)

    radius = radius + fwoth
    call obuild(outfile,'Outboard first wall',fwoth,radius)

    radius = radius + blnkoth
    call obuild(outfile,'Outboard blanket',blnkoth,radius)

    radius = radius + shldoth
    call obuild(outfile,'Outboard shield',shldoth,radius)

    radius = radius + ddwi
    call obuild(outfile,'Vacuum vessel',ddwi,radius)

    radius = radius + gapsto
    call obuild(outfile,'Gap',gapsto,radius)

    radius = radius + tfthko
    call obuild(outfile,'TF coil outboard leg',tfthko,radius)

    !  Port size information

    call osubhd(outfile,'Port Size Information :')
    call ovarre(outfile,'Port width (m)','(prtsz)',prtsz)
    call ovarre(outfile,'Port requirement for beams (m)','(prtszreq)', &
         prtszreq)

  end subroutine stbild

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stphys

    !+ad_name  stphys
    !+ad_summ  Routine to calculate stellarator plasma physics information
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the physics quantities relevant to
    !+ad_desc  a stellarator device.
    !+ad_prob  None
    !+ad_call  beamfus
    !+ad_call  betcom
    !+ad_call  palph
    !+ad_call  palph2
    !+ad_call  pcond
    !+ad_call  phyaux
    !+ad_call  radpwr
    !+ad_call  rether
    !+ad_call  stblim
    !+ad_call  stdlim
    !+ad_call  stheat
    !+ad_hist  29/06/94 PJK Initial version
    !+ad_hist  16/01/96 PJK Modifications in the light of D-He3 changes
    !+ad_hisc               (idhe3 is always set to zero for stellarators)
    !+ad_hist  10/06/96 PJK Added use of IWALLD in wall load calculation
    !+ad_hist  01/04/98 PJK Modified BETCOM and PCOND calls
    !+ad_hist  01/04/98 PJK Modified BETCOM call
    !+ad_hist  24/04/98 PJK Modified BETCOM call
    !+ad_hist  30/06/98 PJK Modified PCOND call
    !+ad_hist  19/01/99 PJK Modified PCOND call
    !+ad_hist  16/07/01 PJK Modified PCOND call
    !+ad_hist  22/05/06 PJK Modified PALPH2 call
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added physics_module
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  17/12/12 PJK Added zfear to betcom, radpwr argument lists
    !+ad_hist  23/01/13 PJK Modified poloidal field calculation to use iotabar;
    !+ad_hisc               Changed PCOND q95 argument to iotabar
    !+ad_hist  12/06/13 PJK taup now global
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: fusrat,pht,powht,sbar,sigvdt,zion

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (idhe3 == 1) ftr = max(ftrit,1.0D-6)

    !  Calculate plasma composition

    call betcom(alphan,alphat,cfe0,dene,fdeut,ftrit,fhe3,ftr,ftritbm, &
         idhe3,ignite,impc,impfe,impo,ralpne,rnbeam,te,zeff,abeam, &
         afuel,aion,deni,dlamee,dlamie,dnalp,dnbeam,dnitot,dnla, &
         dnprot,dnz,falpe,falpi,pcoef,rncne,rnone,rnfene,zeffai,zion,zfear)

    ten = te * pcoef
    tin = ti * pcoef

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

    call palph(alphan,alphat,deni,ealpha,fdeut,fhe3,ftr,ftrit, &
         idhe3,iiter,pcoef,ti,palp,pcharge,pneut,sigvdt)

    !  Calculate neutral beam slowing down effects
    !  If ignited, then ignore beam fusion effects

    if ((pnbeam /= 0.0D0).and.(ignite == 0)) then
       call beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie, &
            ealpha,enbeam,fdeut,ftrit,ftritbm,sigvdt,ten,tin,vol, &
            zeffai,betanb,dnbeam2,palpnb)
    end if

    call palph2(bt,bp,dene,deni,dnitot,ftr,falpe,falpi,palpnb, &
         ifalphap,pcharge,pcoef,pneut,te,ti,vol,alpmw,betaft, &
         palp,palpi,palpe,pfuscmw,powfmw)

    !  Neutron wall load

    if (iwalld == 1) then
       wallmw = ffwal * (pneut*vol) / sarea
    else
       wallmw = ffwal * (pneut*vol) / fwarea
    end if

    !  Calculate ion/electron equilibration power

    call rether(alphan,alphat,dene,dlamie,te,ti,zeffai,pie)

    !  Calculate radiation power

    call radpwr(alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95,rmajor, &
         rminor,ralpne,rncne,rnone,rnfene,ssync,ten,vol,pbrem,plrad, &
         prad,psync,zfear)

    !  Limit for minimum radiation power

    pht = 1.0D-6 * (pinji + pinje) + alpmw + pcharge*vol
    pbrem = max( (fradmin*pht/vol), pbrem)
    prad = pbrem + psync

    !  Heating power to plasma

    powht = alpmw + pcharge*vol + (pinje+pinji)*1.0D-6 + pohmpv*vol &
         - prad*vol

    !  Power to divertor

    pdivt = powht - plrad*vol
    pdivt = max(0.001D0, pdivt)

    !  Calculate density limit

    call stdlim(alphan,bt,powht,rmajor,rminor,dnelimt)

    !  Calculate transport losses and energy confinement time using the
    !  chosen scaling law
    !  N.B. iotabar replaces tokamak q95 in argument list

    call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hfact, &
         iinvqd,isc,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji, &
         plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,iotabar,qstar,vol, &
         xarea,zeff,ptre,ptri,tauee,tauei,taueff,powerht)

    !  Calculate auxiliary physics related information
    !  for the rest of the code

    sbar = 1.0D0
    call phyaux(aspect,dene,deni,idhe3,plascur,powfmw,sbar,dnalp, &
         dnprot,taueff,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

    !  Calculate beta limit

    call stblim(betalim)

  end subroutine stphys

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stheat(outfile,iprint)

    !+ad_name  stheat
    !+ad_summ  Routine to calculate the auxiliary heating power
    !+ad_summ  in a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the auxiliary heating power for
    !+ad_desc  a stellarator device.
    !+ad_prob  None
    !+ad_call  culnbi
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  29/06/94 PJK Initial version
    !+ad_hist  01/04/98 PJK Modified call to CULNBI
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  17/10/12 PJK Added current_drive_module
    !+ad_hist  31/10/12 PJK Added stellarator_variables
    !+ad_hist  23/01/13 PJK Added comment about ignited plasma
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)), save :: effnbss,fpion,fshine

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (iprint /= 1) then

       !  Assign heating power to the desired mechanism

       select case (isthtr)

       case (1)  !  Electron cyclotron resonance heating

          echpwr = pheat
          pinji = 0.0D0
          pinje = echpwr

       case (2)  !  Lower Hybrid heating

          plhybd = pheat
          pinji = 0.0D0
          pinje = plhybd

       case (3)  !  Neutral beam injection heating

          !  Use routine described in AEA FUS 172, but discard the current
          !  drive efficiency as this is irrelevant for stellarators. We are
          !  only really interested in fpion, fshine and taubeam.

          call culnbi( &
               abeam,alphan,alphat,aspect,dene,deni,dlamie,dnla,enbeam, &
               eps,feffcd,frbeam,ftr,ralpne,rmajor,rminor,rncne,rnfene, &
               rnone,te,ten,zeff,zeffai,effnbss,fpion,fshine,taubeam)

          pnbeam = pheat
          pinji = pnbeam * fpion
          pinje = pnbeam * (1.0D0-fpion)

       case default

          write(*,*) 'Error in routine STHEAT:'
          write(*,*) 'Illegal value for switch ISTHTR, = ',isthtr
          write(*,*) 'PROCESS stopping.'
          stop

       end select

       !  Calculate neutral beam current

       if (abs(pnbeam) > 1.0D-8) then
          cnbeam = 1.0D-3 * pnbeam / enbeam
       else
          cnbeam = 0.0D0
       end if

       !  Ratio of fusion to injection power

       if ((pinje+pinji) == 0.0D0) then
          bigq = 1.0D18
       else
          bigq = 1.0D6*powfmw / (pinje+pinji)
       end if

    end if

    if ((iprint == 0).or.(sect04 == 0)) return

    !  Output section

    call oheadr(outfile,'Auxiliary Heating System')

    select case (isthtr)
    case (1)
       call ocmmnt(outfile,'Electron Cyclotron Resonance Heating')
    case (2)
       call ocmmnt(outfile,'Lower Hybrid Heating')
    case (3)
       call ocmmnt(outfile,'Neutral Beam Injection Heating')
    case default
       write(*,*) 'Error in routine STHEAT:'
       write(*,*) 'Illegal value of ISTHTR, = ',isthtr
       write(*,*) 'PROCESS stopping.'
       stop
    end select
    if (ignite == 1) then
       call ocmmnt(outfile, &
            'Ignited plasma; injected power only used for start-up phase')
    end if
    call oblnkl(outfile)

    call ovarre(outfile,'Auxiliary power supplied to plasma (W)', &
         '(pheat)',pheat)
    call ovarre(outfile,'Energy multiplication factor Q','(bigq)',bigq)

    if (abs(pnbeam) > 1.0D-8) then
       call ovarre(outfile,'Neutral beam energy (keV)','(enbeam)',enbeam)
       call ovarre(outfile,'Neutral beam current (A)','(cnbeam)',cnbeam)
       call ovarre(outfile,'Fraction of beam energy to ions','(fpion)', &
            fpion)
       call ovarre(outfile,'Neutral beam shine-through','(fshine)', &
            fshine)
       call ovarre(outfile,'R injection tangent / R-major','(frbeam)', &
            frbeam)
       call ovarre(outfile,'Beam decay lengths to centre','(taubeam)', &
            taubeam)
    end if

  end subroutine stheat

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stcoil(outfile,iprint)

    !+ad_name  stcoil
    !+ad_summ  Routine that performs the calculations for stellarator TF coils
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the properties of the TF coils for
    !+ad_desc  a stellarator device.
    !+ad_prob  None
    !+ad_call  outtf
    !+ad_call  stclen
    !+ad_call  stcshp
    !+ad_call  stresscl
    !+ad_call  tfcind
    !+ad_hist  20/07/94 PJK Initial version
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  18/10/12 PJK Modified argument list of tfcind
    !+ad_hist  29/10/12 PJK Added sctfcoil_module
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  23/01/13 PJK TFTORT now assumed to be the input value unless
    !+ad_hisc               limited by space available
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(kind(1.0D0)) :: awpc,awptf,leni,leno,rbcndut,rcoil,tftort2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Produce output to file if required

    if (iprint == 1) then
       call outtf(outfile)
       return
    end if

    !  Cross-sectional area of outboard leg
    !  Same strange algorithm as the original version in SCTFCOIL

    arealeg = 4.0D0 * tfthko * (rtot-0.5D0*tfthko) * sin(pi/tfno)

    !  Radius of centre of inboard leg

    rtfcin = bore + ohcth + gapoh + bcylth + 0.5D0*tfcth

    !  Radius of outer edge of inboard leg

    rcoil = rtfcin + 0.5D0*tfcth

    !  Unlike in a tokamak, the inboard legs do not necessarily form a
    !  continuous ring. The maximum value of tftort is calculated ensuring
    !  that adjacent coils are not wider than their inboard edge centres are
    !  apart.

    !  Thickness of inboard leg in toroidal direction

    tftort2 = min(tftort, 2.0D0*(rcoil-tfcth)*tan(pi/tfno))

    !  Total area of inboard legs (coils have rectangular cross-section)

    tfareain = tfno * tfcth * tftort2

    !  Total current in TF coils

    ritfc = oacdcp * tfareain

    !  Peak toroidal field and radius of its occurrence
    !  Tokamak values; may be inaccurate for non-planar TF coils

    rbmax = rcoil
    bmaxtf = ritfc * rmu0 / (2.0D0*pi*rbmax)

    !  Peak field including ripple (arbitrarily set at 9 per cent)

    bmaxtfrp = bmaxtf * 1.09D0

    !  Centering force

    cforce = bmaxtf * ritfc / (2.0D0*tfno)

    !  Vertical force

    vforce = 0.5D0 * bt * rmajor * 0.5D0*ritfc * log(rtot/rtfcin) &
         / tfno

    !  Horizontal and vertical bores

    tfboreh = rtot - rtfcin - 0.5D0*(tfthko+tfcth)
    borev = 2.0D0 * hmax

    !  Determine coil shape

    call stcshp

    !  Calculate TF coil self inductance

    call tfcind(tfcth)

    !  TF coil energy (0.5*L*I**2) (GJ)

    estotf = 1.0D-9 * 0.5D0*tfind / tfno * ritfc**2

    !  Calculate length of a TF coil

    call stclen(tfleng)

    tfleng = tfleng / tfno

    !  Modify case thickness if simple stress model is to be used

    if (itfmod /= 1) thkcas = tfcth*0.5D0

    !  Winding pack dimensions
    !  A simple rectangular winding pack is assumed

    !  Radial dimension

    thkwp = tfcth - thkcas - 2.0D0*tinstf - casthi

    !  'Toroidal' dimension

    wwp1 = tftort2 - 2.0D0*(casths+tinstf)

    !  Cross-sectional area

    awptf = thkwp * wwp1

    !  Cross-sectional area including insulation

    awpc = (thkwp + 2.0D0*tinstf) * (wwp1 + 2.0D0*tinstf)

    !  Cross-sectional area of surrounding case

    acasetf = tfcth*tftort2 - awpc

    !  Checks for negative lengths or areas

    if ( (thkwp   <= 0.0D0).or. &
         (wwp1    <= 0.0D0).or. &
         (awptf   <= 0.0D0).or. &
         (awpc    <= 0.0D0).or. &
         (acasetf <= 0.0D0) ) then
       write(*,*) 'Error in routine STCOIL:'
       write(*,*) 'Winding pack cross-section problem'
       write(*,*) 'tftort2 = ',tftort2
       write(*,*) 'thkwp = ',thkwp
       write(*,*) 'wwp1 = ',wwp1
       write(*,*) 'awptf = ',awptf
       write(*,*) 'awpc = ',awpc
       write(*,*) 'acasetf = ',acasetf
       write(*,*) ' '
       write(*,*) 'PROCESS continuing...'
    end if

    !  Winding pack current density

    jwptf = ritfc/(tfno*awptf)

    !  Superconducting cable information

    !  Radius of rounded corners of cable space inside conduit

    rbcndut = thwcndut * 0.75D0

    !  Dimension of square cross-section of each turn

    leno = sqrt(cpttf / jwptf)

    !  Dimension of square cable space inside insulation and case of
    !  the conduit of each turn

    leni = leno - 2.0D0*(thwcndut + thicndut)

    if (leni <= 0.0D0) then
       write(*,*) 'Error in routine STCOIL:'
       write(*,*) 'Cable space dimension, leni = ',leni
       write(*,*) 'Reduce conduit case or insulation thicknesses,'
       write(*,*) 'or increase cpttf value or lower bound.'
       write(*,*) ' '
    end if

    !  Cross-sectional area of cable space per turn

    acstf = sign(1.0D0,leni) * ( leni**2 - (4.0D0-pi)*rbcndut**2 )

    if (acstf <= 0.0D0) then
       if (leni < 0.0D0) then
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

    !  Cross-sectional area of conduit case per turn

    acndttf = (leni + 2.0D0*thwcndut)**2 - acstf

    !  Total number of turns per TF coil

    turnstf = ritfc / (cpttf * tfno)

    !  Total conductor cross-sectional area, taking account of void area

    acond = acstf * turnstf * (1.0D0 - vftf)

    !  Void area in cable, for He

    avwp = acstf * turnstf * vftf

    !  Insulation area (not including ground-wall)

    aiwp = turnstf * (leno**2 - acndttf - acstf)

    !  Structure area for cable

    aswp = (turnstf*acndttf)

    !  Half-width of side of coil nearest torus centreline

    tfocrn = 0.5D0 * tftort2

    !  Half-width of side of coil nearest plasma

    tficrn = 0.5D0 * tftort2

    !  Total surface area of coil side facing plasma: inboard region

    tfsai = 4.0D0 * tfno * tficrn * hr1

    !  Total surface area of coil side facing plasma: outboard region

    tfsao = 2.0D0 * tfno * tficrn * (tfleng - 2.0D0*hr1)

    !  Mass of case. Correction factor included to maintain enough
    !  structure when the case is a small fraction of the total coil

    whtcas = casfact * tfleng * acasetf * dcase * &
         sqrt(0.41D0 * tfareain/(acasetf*tfno))

    !  Masses of conductor constituents

    !  Superconductor

    whtconsc = tfleng * turnstf * acstf*(1.0D0-vftf) * &
         (1.0D0-fcutfsu)*dcond(isumattf)

    !  Copper

    whtconcu = tfleng * turnstf * acstf*(1.0D0-vftf) * &
         fcutfsu*dcopper

    !  Steel conduit (sheath)

    whtconsh = tfleng * turnstf * acndttf * denstl

    !  Total conductor mass

    whtcon = whtconsc + whtconcu + whtconsh

    !  Bucking cylinder mass (assumed not to exist)

    wtbc = 0.0D0

    !  Total TF coil mass

    whttf = (whtcas+whtcon) * tfno

    !  Perform stress calculations

    call stresscl

  end subroutine stcoil

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stcshp

    !+ad_name  stcshp
    !+ad_summ  Routine that approximates the stellarator TF coil shape
    !+ad_summ  in the poloidal plane
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine approximates the shape of the stellarator TF coils
    !+ad_desc  using four circular arcs along the edge facing the plasma.
    !+ad_desc  The coils are assumed to be elliptical in cross-section, with an
    !+ad_desc  elongation ymax/xmax of 1.1.
    !+ad_desc  <P>The code is almost identical to that in routine
    !+ad_desc  <A HREF="coilshap.html">COILSHAP</A>.
    !+ad_prob  This is clearly a highly dodgy approximation...
    !+ad_call  None
    !+ad_hist  20/07/94 PJK Initial version
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: acoil, thet2, thet3, thet4

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Mean coil minor radius

    acoil = 0.5D0 * ((rtot - 0.5D0*tfthko) - (rtfcin + 0.5D0*tfcth))

    !  Point on inboard midplane

    xarc(1) = rtfcin + 0.5D0*tfcth
    yarc(1) = 0.0D0

    !  Point at top of coil

    xarc(3) = (rtfcin + 0.5D0*tfcth) + acoil
    yarc(3) = hmax*1.1D0

    !  Point on outboard midplane

    xarc(5) = rtot - 0.5D0*tfthko
    yarc(5) = 0.0D0

    !  Point on inboard edge

    xarc(2) = xarc(3) - sqrt(0.5555555555D0*acoil*acoil)
    yarc(2) = 0.6666666666D0 * yarc(3)

    !  Point on outboard edge

    xarc(4) = xarc(3) + sqrt(0.5555555555D0*acoil*acoil)
    yarc(4) = 0.6666666666D0 * yarc(3)

    !  Find arc centres

    yctfc(4) = 0.0D0
    xctfc(4) = 0.5D0*(xarc(5)**2 - xarc(4)**2 - yarc(4)**2) &
         /(xarc(5) - xarc(4))
    thet4 = atan2(yarc(4) , (xarc(4)-xctfc(4)) )
    dthet(4) = abs(thet4)
    radctf(4) = sqrt( (yarc(4)-yctfc(4))**2 + (xarc(4)-xctfc(4))**2)

    xctfc(3) = (2.0D0*(yarc(4) - yarc(3)) * (yarc(4)-tan(thet4)*xarc(4)) &
         + xarc(3)**2 + yarc(3)**2 - xarc(4)**2 - yarc(4)**2 ) &
         /2.0D0 / ( (xarc(3) - xarc(4)) - (yarc(4) - yarc(3))*tan(thet4) )
    yctfc(3) = yarc(4) - tan(thet4) * (xarc(4) - xctfc(3))
    thet3 = atan2( (yarc(3) - yctfc(3)), (xarc(3) - xctfc(3)))
    dthet(3) = abs(thet3 - thet4)
    radctf(3) = sqrt( (yarc(3)-yctfc(3))**2 + (xarc(3)-xctfc(3))**2)

    xctfc(2) = (2.0D0*(yarc(3) - yarc(2)) * (yarc(3)-tan(thet3)*xarc(3)) &
         + xarc(2)**2 + yarc(2)**2 - xarc(3)**2 - yarc(3)**2 ) &
         /2.0D0 / ( (xarc(2) - xarc(3)) - (yarc(3) - yarc(2))*tan(thet3) )
    yctfc(2) = yarc(3) - tan(thet3) * (xarc(3) - xctfc(2))
    thet2 = atan2( (yarc(2) - yctfc(2)), (xarc(2) - xctfc(2)) )
    dthet(2) = abs(abs(thet2) - thet3)
    radctf(2) = sqrt( (yarc(2)-yctfc(2))**2 + (xarc(2)-xctfc(2))**2 )

    xctfc(1) = ( xarc(2)**2 - xarc(1)**2 + yarc(2)**2) / &
         (2.0D0*(xarc(2)-xarc(1)))
    yctfc(1) = 0.0D0
    radctf(1) = xctfc(1) - xarc(1)
    dthet(1) = atan2(yarc(2), (xctfc(1)-xarc(1)))

    !  Half-height of TF coil inboard leg 'straight section'

    hr1 = yarc(2)

  end subroutine stcshp

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stclen(totlen)

    !+ad_name  stclen
    !+ad_summ  Routine that calculates the total length of the coils
    !+ad_summ  in a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  rzp
    !+ad_cont  rzpxyz
    !+ad_cont  stcdat
    !+ad_args  totlen : output real : Total coil length (m)
    !+ad_desc  This routine calculates the total length of the coils in
    !+ad_desc  a stellarator.
    !+ad_desc  <P>The coil geometry is based on that of the Wendelstein VII-X
    !+ad_desc  design, with five different coils being repeated ten times to
    !+ad_desc  produce the complete 50-coil set.
    !+ad_desc  <P>The data are scaled with the machine's average major radius and
    !+ad_desc  the mean coil minor radius.
    !+ad_prob  None
    !+ad_call  rzp
    !+ad_call  rzpxyz
    !+ad_call  stcdat
    !+ad_hist  11/07/94 PJK Initial version
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  15/05/13 PJK Swapped build order of vacuum vessel and gap
    !+ad_stat  Okay
    !+ad_docs  Wendelstein VII-X: Application for Preferential Support, Aug 1990
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: totlen

    !  Local variables

    real(kind(1.0D0)) :: a0,a0i,a0o,dlen,pp,rp,x0,x1,x2,y0,y1,y2,z0,z1,z2,zp
    real(kind(1.0D0)), dimension(58,5) :: phi
    real(kind(1.0D0)), dimension(58) :: theta
    integer :: icoil,itheta

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Coil mean minor radius (m) --- average of inboard and outboard

    a0i = 0.5D0*tfcth + gapds + ddwi + shldith + blnkith + fwith + &
         scrapli + rminor

    a0o = rminor + scraplo + fwoth + blnkoth + shldoth + ddwi + &
         gapsto + 0.5D0*tfthko

    a0 = 0.5D0*(a0i + a0o)

    !  Read in coil geometry as a function of (theta,phi)

    call stcdat(theta,phi)

    totlen = 0.0D0

    do icoil = 1,5

       !  Find (x,y,z) of first point on coil

       call rzp(phi(1,icoil),theta(1),rmajor,a0,rp,zp,pp)
       call rzpxyz(rp,zp,pp,x0,y0,z0)

       x1 = x0 ; y1 = y0 ; z1 = z0

       do itheta = 2,58

          !  Convert from (phi,theta) to (R,Z,phi)

          call rzp(phi(itheta,icoil),theta(itheta),rmajor,a0,rp,zp,pp)

          !  Convert from (R,Z,phi) to (x,y,z)

          call rzpxyz(rp,zp,pp,x2,y2,z2)

          dlen = sqrt((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)

          totlen = totlen + dlen

          x1 = x2 ; y1 = y2 ; z1 = z2

       end do

       !  Add distance between first and last points on coil

       dlen = sqrt((x0-x1)**2 + (y0-y1)**2 + (z0-z1)**2)
       totlen = totlen + dlen

    end do

    !  Multiply by ten to obtain total length for whole 50-coil set

    totlen = totlen*10.0D0

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine rzp(phi,theta,r0,a0,rp,zp,pp)

      !+ad_name  rzp
      !+ad_summ  Routine to convert the (phi,theta) coordinates defining
      !+ad_summ  the position of a point on the stellarator coils
      !+ad_summ  to (R,Z,phi) coordinates, given the average major and
      !+ad_summ  minor radii
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  phi   : input real : Toroidal angle of point on coil (rad)
      !+ad_args  theta : input real : Poloidal angle of point on coil (rad)
      !+ad_args  r0    : input real : Coil mean major radius (m)
      !+ad_args  a0    : input real : Coil mean minor radius (m)
      !+ad_args  rp    : output real : Major radius of point (m)
      !+ad_args  zp    : output real : Vertical coordinate of point (m)
      !+ad_args  pp    : output real : Toroidal angle of point (rad)
      !+ad_desc  This routine converts the (phi,theta) coordinates defining
      !+ad_desc  the position of a point on the stellarator coils to
      !+ad_desc  (R,Z,phi) coordinates, given the average major and minor radii.
      !+ad_desc  <P>The (phi,theta) data are taken from the Wendelstein VII-X
      !+ad_desc  geometry, as is the locus of the minor axis. However, the
      !+ad_desc  (R,Z) cross-section of the surface on which the coils lie
      !+ad_desc  is complicated in W VII-X, so this is approximated here by
      !+ad_desc  an ellipse whose major axis rotates continuously with toroidal angle.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  12/07/94 PJK Initial version
      !+ad_hist  20/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Wendelstein VII-X: Application for Preferential Support, Aug 1990
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: a0,phi,r0,theta
      real(kind(1.0D0)), intent(out) :: pp,rp,zp

      !  Local variables

      real(kind(1.0D0)), parameter :: pio2 = 1.5707963267948966D0
      real(kind(1.0D0)) :: a,da,rma,zma

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  (R,Z) of minor axis

      rma = r0 &
           + 0.0775D0*r0*cos(5.0D0*phi) &
           - 1.8537D-3*r0*cos(10.0D0*phi) &
           + 3.0595D-4*r0*cos(17.0D0*phi)

      zma = 0.0632D0*r0*sin(5.0D0*phi) &
           + 1.2058D-3*r0*sin(10.0D0*phi)

      !  Approximation of (R,Z) cross-section of surface on which coil
      !  conductors lie. This describes an ellipse that rotates with
      !  toroidal angle.

      da = 0.1D0 * a0

      a = a0 + da*sin(2.0D0*theta + 5.0D0*phi - pio2)

      !  Corrected (R,Z,phi) coordinates

      rp = rma + a*cos(theta)
      zp = zma + a*sin(theta)
      pp = phi

    end subroutine rzp

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine rzpxyz(rp,zp,pp,x,y,z)

      !+ad_name  rzpxyz
      !+ad_summ  Routine to convert from (R,Z,phi) coordinates to Cartesian (x,y,z)
      !+ad_summ  coordinates
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  rp : input real : Radial coordinate (m)
      !+ad_args  zp : input real : Vertical coordinate (m)
      !+ad_args  pp : input real : Toroidal angle coordinate (rad)
      !+ad_args  x  : output real : Cartesian X coordinate (m)
      !+ad_args  y  : output real : Cartesian Y coordinate (m)
      !+ad_args  z  : output real : Cartesian Z coordinate (m)
      !+ad_desc  This routine converts from (R,Z,phi) coordinates to
      !+ad_desc  Cartesian (x,y,z) coordinates.
      !+ad_desc  (R,0,0) corresponds to the x-axis.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  12/07/94 PJK Initial version
      !+ad_hist  20/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: rp,zp,pp
      real(kind(1.0D0)), intent(out) :: x,y,z

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      x = rp*cos(pp)
      y = rp*sin(pp)
      z = zp

    end subroutine rzpxyz

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine stcdat(theta,phi)

      !+ad_name  stcdat
      !+ad_summ  Routine to load stored coil geometry data into arrays
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  theta(58) : output real array : poloidal angle values
      !+ad_argc                for points on the stellarator coils
      !+ad_args  phi(58,5) : output real array : toroidal angle values
      !+ad_argc                for points on the stellarator coils
      !+ad_desc  This routine loads stored coil geometry data into arrays
      !+ad_desc  theta and phi.
      !+ad_desc  <P>The data are estimated from pictures of the Wendelstein
      !+ad_desc  VII-X coil set.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  11/07/94 PJK Initial version
      !+ad_hist  20/09/12 PJK Initial F90 version
      !+ad_hist  16/10/12 PJK Added constants
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), dimension(58), intent(out) :: theta
      real(kind(1.0D0)), dimension(58,5), intent(out) :: phi

      !  Local variables

      real(kind(1.0D0)), dimension(58,5) :: philoc
      integer :: itheta

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Toroidal angles as a function of theta, around each coil

      data (philoc(itheta,1), itheta=1,58)/ &
           5.79986D-02, 5.79986D-02, 5.79986D-02, 5.31654D-02, &
           5.31654D-02, 4.83322D-02, 4.83322D-02, 4.34990D-02, &
           3.38325D-02, 2.41661D-02, 1.44997D-02, 4.83322D-03, &
           0.00000D+00,-1.44997D-02,-2.41661D-02,-2.41661D-02, &
           -2.41661D-02,-2.41661D-02,-1.93329D-02,-1.93329D-02, &
           -2.89993D-02,-3.38325D-02,-4.83322D-02,-5.31654D-02, &
           -4.34990D-02,-2.89993D-02,-9.66644D-03, 9.66644D-03, &
           2.89993D-02, 4.83322D-02, 6.76651D-02, 8.21647D-02, &
           1.01498D-01, 1.15997D-01, 1.35330D-01, 1.49830D-01, &
           1.59496D-01, 1.59496D-01, 1.44997D-01, 1.35330D-01, &
           1.30497D-01, 1.25664D-01, 1.25664D-01, 1.25664D-01, &
           1.25664D-01, 1.25664D-01, 1.15997D-01, 1.11164D-01, &
           1.06331D-01, 1.01498D-01, 9.66644D-02, 8.69980D-02, &
           7.73315D-02, 7.73315D-02, 7.24983D-02, 6.76651D-02, &
           6.76651D-02, 6.28319D-02/

      data (philoc(itheta,2), itheta=1,58)/ &
           1.93329D-01, 1.88496D-01, 1.83662D-01, 1.83662D-01, &
           1.78829D-01, 1.73996D-01, 1.69163D-01, 1.64329D-01, &
           1.64329D-01, 1.59496D-01, 1.54663D-01, 1.49830D-01, &
           1.44997D-01, 1.40163D-01, 1.35330D-01, 1.25664D-01, &
           1.15997D-01, 1.06331D-01, 1.01498D-01, 9.66644D-02, &
           9.66644D-02, 9.18312D-02, 9.18312D-02, 8.69980D-02, &
           7.24983D-02, 6.28319D-02, 5.79986D-02, 6.28319D-02, &
           7.73315D-02, 9.66644D-02, 1.15997D-01, 1.35330D-01, &
           1.49830D-01, 1.64329D-01, 1.78829D-01, 1.93329D-01, &
           2.12662D-01, 2.31995D-01, 2.46494D-01, 2.56161D-01, &
           2.60994D-01, 2.51327D-01, 2.36828D-01, 2.27161D-01, &
           2.17495D-01, 2.12662D-01, 2.12662D-01, 2.07828D-01, &
           2.07828D-01, 2.07828D-01, 2.07828D-01, 2.07828D-01, &
           2.07828D-01, 2.07828D-01, 2.02995D-01, 2.02995D-01, &
           1.98162D-01, 1.98162D-01/

      data (philoc(itheta,3), itheta=1,58)/ &
           3.18992D-01, 3.23826D-01, 3.28659D-01, 3.33492D-01, &
           3.33492D-01, 3.33492D-01, 3.28659D-01, 3.23826D-01, &
           3.18992D-01, 3.09326D-01, 2.99660D-01, 2.94826D-01, &
           2.89993D-01, 2.89993D-01, 2.85160D-01, 2.80327D-01, &
           2.80327D-01, 2.75494D-01, 2.70660D-01, 2.60994D-01, &
           2.51327D-01, 2.41661D-01, 2.31995D-01, 2.17495D-01, &
           2.07828D-01, 1.98162D-01, 1.83662D-01, 1.64329D-01, &
           1.59496D-01, 1.59496D-01, 1.64329D-01, 1.78829D-01, &
           1.93329D-01, 2.12662D-01, 2.27161D-01, 2.41661D-01, &
           2.56161D-01, 2.70660D-01, 2.80327D-01, 3.04493D-01, &
           3.14159D-01, 3.23826D-01, 3.38325D-01, 3.47992D-01, &
           3.67325D-01, 3.86658D-01, 3.86658D-01, 3.57658D-01, &
           3.18992D-01, 2.99660D-01, 2.89993D-01, 2.94826D-01, &
           2.94826D-01, 2.99660D-01, 3.09326D-01, 3.14159D-01, &
           3.18992D-01, 3.18992D-01/

      data (philoc(itheta,4), itheta=1,58)/ &
           4.15657D-01, 4.25323D-01, 4.34990D-01, 4.44656D-01, &
           4.54323D-01, 4.63989D-01, 4.68822D-01, 4.73656D-01, &
           4.73656D-01, 4.68822D-01, 4.63989D-01, 4.54323D-01, &
           4.49489D-01, 4.44656D-01, 4.39823D-01, 4.39823D-01, &
           4.34990D-01, 4.34990D-01, 4.34990D-01, 4.30157D-01, &
           4.25323D-01, 4.15657D-01, 4.01157D-01, 3.86658D-01, &
           3.67325D-01, 3.47992D-01, 3.38325D-01, 3.23826D-01, &
           3.18992D-01, 3.09326D-01, 2.94826D-01, 2.85160D-01, &
           2.75494D-01, 2.75494D-01, 2.80327D-01, 2.89993D-01, &
           2.99660D-01, 3.14159D-01, 3.28659D-01, 3.43159D-01, &
           3.52825D-01, 3.67325D-01, 3.81824D-01, 3.91491D-01, &
           4.01157D-01, 4.15657D-01, 4.34990D-01, 4.54323D-01, &
           4.73656D-01, 4.88155D-01, 4.97822D-01, 4.83322D-01, &
           4.10824D-01, 3.91491D-01, 3.91491D-01, 3.96324D-01, &
           4.05990D-01, 4.15657D-01/

      data (philoc(itheta,5), itheta=1,58)/ &
           5.02655D-01, 5.07488D-01, 5.17154D-01, 5.31654D-01, &
           5.41321D-01, 5.55820D-01, 5.70320D-01, 5.89653D-01, &
           6.04152D-01, 6.13819D-01, 6.23485D-01, 6.28319D-01, &
           6.28319D-01, 6.23485D-01, 6.18652D-01, 6.13819D-01, &
           6.08986D-01, 6.04152D-01, 5.99319D-01, 5.99319D-01, &
           5.94486D-01, 5.89653D-01, 5.84820D-01, 5.79986D-01, &
           5.70320D-01, 5.55820D-01, 5.36487D-01, 5.21988D-01, &
           5.02655D-01, 4.83322D-01, 4.73656D-01, 4.68822D-01, &
           4.63989D-01, 4.63989D-01, 4.59156D-01, 4.54323D-01, &
           4.49489D-01, 4.34990D-01, 4.25323D-01, 4.20490D-01, &
           4.20490D-01, 4.25323D-01, 4.34990D-01, 4.39823D-01, &
           4.54323D-01, 4.73656D-01, 4.92988D-01, 5.12321D-01, &
           5.31654D-01, 5.46154D-01, 5.65487D-01, 5.79986D-01, &
           5.89653D-01, 5.94486D-01, 5.89653D-01, 5.60653D-01, &
           5.21988D-01, 5.12321D-01/

      !  Calculate poloidal angles

      do itheta = 1,58
         theta(itheta) = dble(itheta)*twopi/58.0D0
      end do

      !  Store toroidal angles

      phi(:,:) = philoc(:,:)

    end subroutine stcdat

  end subroutine stclen

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stfwbs

    !+ad_name  stfwbs
    !+ad_summ  Routine to calculate first wall, blanket and shield properties
    !+ad_summ  for a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This routine calculates a stellarator's first wall, blanket and
    !+ad_desc  shield properties not already calculated in routine
    !+ad_desc  <A HREF="fwbs.html">fwbs</A>.
    !+ad_desc  <P>In practice, this routine exists purely as
    !+ad_desc  a placeholder for a possible future full calculation.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  01/07/94 PJK Initial version
    !+ad_hist  10/06/96 PJK Moved first wall area calculation into STBILD
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Currently unused
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  end subroutine stfwbs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stdlim(alphan,bt,powht,rmajor,rminor,dlimit)

    !+ad_name  stdlim
    !+ad_summ  Routine to calculate the density limit in a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  alphan : input real : Density profile index
    !+ad_args  bt     : input real : Toroidal field on axis (T)
    !+ad_args  powht  : input real : Absorbed heating power (MW)
    !+ad_args  rmajor : input real : Plasma major radius (m)
    !+ad_args  rminor : input real : Plasma minor radius (m)
    !+ad_args  dlimit : output real : Maximum volume-averaged plasma density (/m3)
    !+ad_desc  This routine calculates the density limit for a stellarator.
    !+ad_prob  None
    !+ad_call  gamfun
    !+ad_hist  30/06/94 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  S.Sudo, Y.Takeiri, H.Zushi et al., Scalings of Energy Confinement
    !+ad_docc  and Density Limit in Stellarator/Heliotron Devices, Nuclear Fusion
    !+ad_docc  vol.30, 11 (1990).
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: alphan,bt,powht,rmajor,rminor
    real(kind(1.0D0)), intent(out) :: dlimit

    !  Local variables

    real(kind(1.0D0)) :: arg,denom,dnlamx

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    arg = powht*bt / (rmajor*rminor*rminor)

    if (arg <= 0.0D0) then
       write(*,*) 'Error in routine STDLIM:'
       write(*,*) 'NaN result (sqrt) will occur, arg = ',arg
       write(*,*) ' powht = ',powht
       write(*,*) '    bt = ',bt
       write(*,*) 'rmajor = ',rmajor
       write(*,*) 'rminor = ',rminor
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    !  Maximum line-averaged electron density

    dnlamx = 0.25D20 * sqrt(arg)

    !  Scale the result so that it applies to the volume-averaged
    !  electron density

    denom = 0.886227D0 * gamfun(alphan+1.0D0) * (1.0D0+alphan)

    if (denom == 0.0D0) then
       write(*,*) 'Error in routine STDLIM:'
       write(*,*) 'Division by zero will occur, denom = ',denom
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    dlimit = dnlamx * gamfun(alphan+1.5D0) / denom

  end subroutine stdlim

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stblim(betamx)

    !+ad_name  stblim
    !+ad_summ  Routine to calculate the beta limit in a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  betamx : output real : Maximum volume-averaged plasma beta
    !+ad_desc  This routine calculates the beta limit for a stellarator.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  30/06/94 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  J.F.Lyon, K.Gulec, R.L.Miller and L.El-Guebaly, Status of the U.S.
    !+ad_docc  Stellarator Reactor Study
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
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

    !+ad_name  stigma
    !+ad_summ  Routine to calculate ignition margin at the final point
    !+ad_summ  with different stellarator confinement time scaling laws
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_desc  This routine calculates the ignition margin at the final
    !+ad_desc  point with different stellarator confinement time scaling laws
    !+ad_prob  None
    !+ad_call  fhfac
    !+ad_call  oblnkl
    !+ad_call  osubhd
    !+ad_call  pcond
    !+ad_hist  30/06/94 PJK Initial version
    !+ad_hist  01/04/98 PJK Modified call to PCOND
    !+ad_hist  30/06/98 PJK Modified call to PCOND
    !+ad_hist  19/01/99 PJK Modified call to PCOND
    !+ad_hist  16/07/01 PJK Modified call to PCOND
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added physics_module
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  23/01/13 PJK Added two more scaling laws; changed PCOND argument
    !+ad_hisc               q95 to iotabar
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    real(kind(1.0D0)) :: d2,powerhtz,ptrez,ptriz,taueez,taueezz, &
         taueffz,taueiz
    integer :: i,iisc
    integer, parameter :: nstlaw = 5
    integer, dimension(nstlaw) :: istlaw

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (sect03 == 0) return

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

       call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,d2, &
            iinvqd,i,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji, &
            plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,iotabar,qstar,vol, &
            xarea,zeff,ptrez,ptriz,taueez,taueiz,taueffz,powerhtz)

       hfac(iisc) = fhfac(i)
       write(outfile,30) tauscl(istlaw(iisc)),taueez,hfac(iisc)
    end do
30  format(t2,a24,t34,f7.3,t58,f7.3)

  end subroutine stigma

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stout(outfile)

    !+ad_name  stout
    !+ad_summ  Routine to print out the final stellarator machine parameters
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_desc  This routine prints out the stellarator's parameters at the
    !+ad_desc  end of a run.
    !+ad_prob  None
    !+ad_call  acpow
    !+ad_call  avail
    !+ad_call  bldgcall
    !+ad_call  costs
    !+ad_call  divcall
    !+ad_call  fispac
    !+ad_call  fwbs
    !+ad_call  loca
    !+ad_call  outplas
    !+ad_call  power2
    !+ad_call  stbild
    !+ad_call  stcoil
    !+ad_call  stheat
    !+ad_call  stigma
    !+ad_call  ststrc
    !+ad_call  tfpwr
    !+ad_call  tfspcall
    !+ad_call  vaccall
    !+ad_hist  30/06/94 PJK Initial version
    !+ad_hist  27/02/96 PJK Added use of IFISPACT
    !+ad_hist  23/01/97 PJK Split routine POWER into POWER1 and POWER2
    !+ad_hist  26/02/97 PJK Added routine LOCA
    !+ad_hist  18/11/97 PJK Removed NOUT argument from FISPAC call
    !+ad_hist  19/05/99 PJK Added call to routine AVAIL
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added costs_module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  17/10/12 PJK Added divertor_module
    !+ad_hist  18/10/12 PJK Added fwbs_module
    !+ad_hist  29/10/12 PJK Added sctfcoil_module
    !+ad_hist  29/10/12 PJK Added vacuum_module
    !+ad_hist  30/10/12 PJK Added power_module
    !+ad_hist  30/10/12 PJK Added buildings_module
    !+ad_hist  23/01/13 PJK Commented out fispac, loca calls
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    call divcall(outfile,1)
    call stbild(outfile,1)
    call stcoil(outfile,1)
    call tfspcall(outfile,1)
    call ststrc(outfile,1)
    call fwbs(outfile,1)

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

    !+ad_name  ststrc
    !+ad_summ  Routine to call the structure module for a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calls the structure module for a stellarator.
    !+ad_desc  This is the stellarator version of routine
    !+ad_desc  <A HREF="strucall.html">STRUCALL</A>.
    !+ad_prob  None
    !+ad_call  struct
    !+ad_hist  01/07/94 PJK Initial version
    !+ad_hist  01/02/96 PJK Added itfsup, ipfres to argument list of STRUCT
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  29/10/12 PJK Added structure_variables
    !+ad_hist  29/10/12 PJK Added structure_module
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: ak,cnorm,twhtpf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Total weight of the PF coil conductor and its structure

    twhtpf = 0.0D0

    !  The intercoil support structure between the TF coils is
    !  proportional to the plasma current normalised to 22 MA.
    !  Since in stellarators, the plasma current is zero but we still
    !  need intercoil support, we will normalise the plasma current
    !  factor in the calculation to unity.

    cnorm = 2.2D7

    !  Unfortunately, setting cnorm to be non-zero causes the PF coil
    !  fence mass to be finite. To fiddle this to be zero, I will set
    !  the kappa argument to zero. (fence mass proportional to kappa)

    ak = 0.0D0

    call struct(cnorm,rmajor,rminor,ak,bt,itfsup,ipfres,tfboreh, &
         hmax,whtshld,divmas,twhtpf,whttf,fwmass,whtblkt,coolmass, &
         wtbc,dewmkg,outfile,iprint,fncmass,aintmass,clgsmass, &
         coldmass,gsmass)

  end subroutine ststrc

end module stellarator_module
