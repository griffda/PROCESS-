!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module global_variables

  !+ad_name  global_variables
  !+ad_summ  Module containing miscellaneous global variables
  !+ad_summ  availability
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains miscellaneous global variables not
  !+ad_desc  well-suited to any of the other 'variables' modules.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  15/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  character(len=48) :: icase = 'PROCESS standard D-T tokamak model'

end module global_variables


module wibble
  real(kind(1.0D0)) :: &
       xtfi,xtfo,xtb,xpf,xdo,xdi,ph,pr,pin,pc,etahp,etainp, &
       etalp,etafp,etacp,fkblkt,sgeff
  common /blkre/ &
       xtfi,xtfo,xtb,xpf,xdo,xdi,ph,pr,pin,pc,etahp,etainp, &
       etalp,etafp,etacp,fkblkt,sgeff

  integer :: nipfwh,nlpfwh,lblnkt,estr,astr,bstr,costr,smstr
  common /blki/ nipfwh,nlpfwh,lblnkt,estr,astr,bstr,costr,smstr
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       admv,clh1,clh2,conv,fndt,hccl,hcwt,pibv,rbrt,rbwt,row, &
       rxcl,shmf,shov,stcl,trcl,wgt,wgt2
  common /bldg1/ &
       admv,clh1,clh2,conv,fndt,hccl,hcwt,pibv,rbrt,rbwt,row, &
       rxcl,shmf,shov,stcl,trcl,wgt,wgt2
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       admvol,convol,cryvol,efloor,elevol,esbldgm3,pfbldgm3, &
       rbvol,rmbvol,shovol,tfcbv,triv,volnucb,volrci,wrbi,wsvol 
  common /bldgv1/ &
       admvol,convol,cryvol,efloor,elevol,esbldgm3,pfbldgm3, &
       rbvol,rmbvol,shovol,tfcbv,triv,volnucb,volrci,wrbi,wsvol 
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       aplasmin,bcylth,blnkith,blnkoth,bore,ddwex,ddwi,gapds, &
       gapoh,gapomin,gapsto,fwarea,fwith,fwoth,hmax,hr1,ohcth, &
       prtsz,prtszreq,rbld,rinboard,rsldi,rsldo,rtfcin,rtot, &
       scrapli,scraplo,shldith,shldoth,shldtth,tfcth,tfootfi, &
       tfthko,vgap,vgaptf,vgap2
  common /build0/ &
       aplasmin,bcylth,blnkith,blnkoth,bore,ddwex,ddwi,gapds, &
       gapoh,gapomin,gapsto,fwarea,fwith,fwoth,hmax,hr1,ohcth, &
       prtsz,prtszreq,rbld,rinboard,rsldi,rsldo,rtfcin,rtot, &
       scrapli,scraplo,shldith,shldoth,shldtth,tfcth,tfootfi, &
       tfthko,vgap,vgaptf,vgap2

  integer :: iohcl
  common /build1/ iohcl

  real(kind(1.0D0)) :: &
       fmsbc,fmsbl,fmsdwe,fmsdwi,fmsfw,fmsoh,fmssh,fmstf
  common /marten/ &
       fmsbc,fmsbl,fmsdwe,fmsdwi,fmsfw,fmsoh,fmssh,fmstf
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       beamwd,bigq,bootipf,bscfmax,cboot,cnbeam,echpwr,&
       echpwr0,echwpow,enbeam,etaech,etalh,etanbi,etaof,&
       feffcd,frbeam,ftritbm,gamcd,pheat,pinjalw,pinje,&
       pinji,plhybd,pnbeam,pofcd,pwplh,pwpnb,taubeam,&
       tbeamin
  common /cdriv0/ &
       beamwd,bigq,bootipf,bscfmax,cboot,cnbeam,echpwr,&
       echpwr0,echwpow,enbeam,etaech,etalh,etanbi,etaof,&
       feffcd,frbeam,ftritbm,gamcd,pheat,pinjalw,pinje,&
       pinji,plhybd,pnbeam,pofcd,pwplh,pwpnb,taubeam,&
       tbeamin

  integer :: iefrf,irfcd
  common /cdriv1/ iefrf,irfcd
!  $Id::                                                                $

!--Version number 1.020
!
!--Description
!  INCLUDE file containing all costs local to the costing routines
!
!--Author
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  14 March 1997
!
!--Reference
!  None
!  
!--History
!  21/12/93 PJK 1.000 Initial version
!  06/03/96 PJK 1.010 Added c2234
!  14/03/97 PJK 1.020 Added c22125,c22126,c22127
!
!--Contents
!  Various cost account values (M$)

  real(kind(1.0D0)) :: &
       c21,c211,c212,c213,c214,c2141,c2142,c215,c216,c217,c2171, &
       c2172,c2173,c2174,c22,c2211,c2212,c22121,c22122,c22123, &
       c22124,c22125,c22126,c22127,c2213,c22131,c22132,c2214,c2215, &
       c2221,c22211,c22212,c22213,c22214,c22215,c2222,c22221,c22222, &
       c22223,c22224,c2223,c223,c2231,c2232,c2233,c2234,c224,c2241, &
       c2242,c2243,c2244,c2245,c2246,c225,c2251,c22511,c22512,c22513, &
       c22514,c22515,c2252,c22521,c22522,c22523,c22524,c22525,c22526, &
       c22527,c2253,c226,c2261,c2262,c2263,c227,c2271,c2272,c2273, &
       c2274,c228,c229,c23,c24,c241,c242,c243,c244,c245,c25,c26,ccont, &
       chx,chxa,cindrt,cpp,cppa
  common /cost2/ &
       c21,c211,c212,c213,c214,c2141,c2142,c215,c216,c217,c2171, &
       c2172,c2173,c2174,c22,c2211,c2212,c22121,c22122,c22123, &
       c22124,c22125,c22126,c22127,c2213,c22131,c22132,c2214,c2215, &
       c2221,c22211,c22212,c22213,c22214,c22215,c2222,c22221,c22222, &
       c22223,c22224,c2223,c223,c2231,c2232,c2233,c2234,c224,c2241, &
       c2242,c2243,c2244,c2245,c2246,c225,c2251,c22511,c22512,c22513, &
       c22514,c22515,c2252,c22521,c22522,c22523,c22524,c22525,c22526, &
       c22527,c2253,c226,c2261,c2262,c2263,c227,c2271,c2272,c2273, &
       c2274,c228,c229,c23,c24,c241,c242,c243,c244,c245,c25,c26,ccont, &
       chx,chxa,cindrt,cpp,cppa
!  $Id::                                                                $

  real(kind(1.0D0)), dimension(4) :: cfind
  real(kind(1.0D0)), dimension(2) :: uchts
  real(kind(1.0D0)), dimension(4) :: ucoam
  real(kind(1.0D0)), dimension(5) :: ucsc
  real(kind(1.0D0)), dimension(2) :: ucturb
  real(kind(1.0D0)), dimension(4) :: ucwst
  common /cost00/ cfind,uchts,ucoam,ucsc,ucturb,ucwst

  real(kind(1.0D0)) :: &
       abktflnc,adivflnc,blkcst,capcost,cdcost,cdirt,cdrlife, &
       cfactr,chplant,coe,coecap,coefuelt,coeoam,concost, &
       cplife,cpstcst,cpstflnc,crctcore,c221,c222,decomf, &
       dintrt,divcst,divlife,dtlife,fcap0,fcap0cp,fcdfuel, &
       fcontng,fcr0,fkind,fwallcst,moneyint,ratecdol,tbktrepl, &
       tcomrepl,tdivrepl,tlife,uubop,uucd,uudiv,uufuel,uufw, &
       uumag,uuves
  common /cost0/ &
       abktflnc,adivflnc,blkcst,capcost,cdcost,cdirt,cdrlife, &
       cfactr,chplant,coe,coecap,coefuelt,coeoam,concost, &
       cplife,cpstcst,cpstflnc,crctcore,c221,c222,decomf, &
       dintrt,divcst,divlife,dtlife,fcap0,fcap0cp,fcdfuel, &
       fcontng,fcr0,fkind,fwallcst,moneyint,ratecdol,tbktrepl, &
       tcomrepl,tdivrepl,tlife,uubop,uucd,uudiv,uufuel,uufw, &
       uumag,uuves

  integer :: iavail,ifueltyp,ipnet,ireactor,lsa
  common /cost1/ iavail,ifueltyp,ipnet,ireactor,lsa

  real(kind(1.0D0)) :: &
       cconfix,cconshpf,cconshtf,cland,cowner,csi,cturbb
  common /ucost0/ &
       cconfix,cconshpf,cconshtf,cland,cowner,csi,cturbb

  real(kind(1.0D0)) :: &
       ucad,ucaf,ucahts,ucap,ucblbe,ucblli,ucblli2o,ucbllipb, &
       ucblss,ucblvd,ucbpmp,ucbus,uccase,ucco,uccpclb,uccpcl1, &
       uccpmp,uccr,uccry,uccryo,uccu,ucdgen,ucdiv,ucdtc,ucduct, &
       ucech,ucel,uces1,uces2,ucfnc,ucfpr,ucfuel,ucfwa,ucfwps, &
       ucfws,ucf1,ucgss,uche3,uchhten,uchhtex,uchlte,uchrs,uchth, &
       uciac,ucich,ucihx,ucint,uclh,uclv,ucmb,ucme
  common /ucost1/ &
       ucad,ucaf,ucahts,ucap,ucblbe,ucblli,ucblli2o,ucbllipb, &
       ucblss,ucblvd,ucbpmp,ucbus,uccase,ucco,uccpclb,uccpcl1, &
       uccpmp,uccr,uccry,uccryo,uccu,ucdgen,ucdiv,ucdtc,ucduct, &
       ucech,ucel,uces1,uces2,ucfnc,ucfpr,ucfuel,ucfwa,ucfwps, &
       ucfws,ucf1,ucgss,uche3,uchhten,uchhtex,uchlte,uchrs,uchth, &
       uciac,ucich,ucihx,ucint,uclh,uclv,ucmb,ucme

  real(kind(1.0D0)) :: &
       ucmisc,ucnbi,ucnbv,ucof,ucpens,ucpfb,ucpfbk,ucpfbs,ucpfcb, &
       ucpfdr1,ucpfic,ucpfps,ucphx,ucpp,ucrb,ucsh,ucshld,ucswyd, &
       uctfbr,uctfbus,uctfdr,uctfgr,uctfic,uctfps,uctfsw,uctpmp, &
       uctr,ucvalv,ucvdsh,ucviac,ucwindpf,ucwindtf,ucws
  common /ucost2/ &
       ucmisc,ucnbi,ucnbv,ucof,ucpens,ucpfb,ucpfbk,ucpfbs,ucpfcb, &
       ucpfdr1,ucpfic,ucpfps,ucphx,ucpp,ucrb,ucsh,ucshld,ucswyd, &
       uctfbr,uctfbus,uctfdr,uctfgr,uctfic,uctfps,uctfsw,uctpmp, &
       uctr,ucvalv,ucvdsh,ucviac,ucwindpf,ucwindtf,ucws
!  $Id::                                                                $

  integer :: divdum
  common /divrti/ divdum

  real(kind(1.0D0)) :: &
       adas,anginc,bpsout,c1div,c2div,c3div,c4div,c5div,c6div,delld, &
       dendiv,densin,divclfr,divdens,divmas,divplt,divsur,fdfs,fdiva, &
       fgamp,fhout,fififi,frrp,hldiv,hldivlim,ksic,lamp,minstang, &
       omegan,omlarg,plsepo,ppdivr,prn1,ptpdiv,rconl,rlclolcn,rlenmax, &
       rsrd,rstrko,tconl,tdiv,tsep,xparain,xpertin,zeffdiv
  common /divrt/ &
       adas,anginc,bpsout,c1div,c2div,c3div,c4div,c5div,c6div,delld, &
       dendiv,densin,divclfr,divdens,divmas,divplt,divsur,fdfs,fdiva, &
       fgamp,fhout,fififi,frrp,hldiv,hldivlim,ksic,lamp,minstang, &
       omegan,omlarg,plsepo,ppdivr,prn1,ptpdiv,rconl,rlclolcn,rlenmax, &
       rsrd,rstrko,tconl,tdiv,tsep,xparain,xpertin,zeffdiv
!  $Id::                                                                $

  integer :: iscenr
  common /est1/ iscenr
!  $Id::                                                                $

!--Version number 1.000
!
!--Description
!  INCLUDE file containing values calculated by FISPACT routines
!
!--Author
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  19 February 1997
!
!--Reference
!  None
!  
!--History
!  06/02/97 PJK 1.000 Initial version
!
!--Contents
!  BLIIZP : Inboard blanket integrated zone power
!  BLIMZP : Inboard blanket mean zone power density
!  BLOIZP : Outboard blanket integrated zone power
!  BLOMZP : Outboard blanket mean zone power density
!  FWIIZP : Inboard first wall integrated zone power
!  FWIMZP : Inboard first wall mean zone power density
!  FWOIZP : Outboard first wall integrated zone power
!  FWOMZP : Outboard first wall mean zone power density
!  BLIACT : Inboard blanket total activity (Bq)
!  BLIGDR : Inboard blanket total gamma dose rate (Sv/hr)
!  BLIHKW : Inboard blanket total heat output (kW)
!  BLOACT : Outboard blanket total activity (Bq)
!  BLOGDR : Outboard blanket total gamma dose rate (Sv/hr)
!  BLOHKW : Outboard blanket total heat output (kW)
!  FWIACT : Inboard first wall total activity (Bq)
!  FWIGDR : Inboard first wall total gamma dose rate (Sv/hr)
!  FWIHKW : Inboard first wall total heat output (kW)
!  FWOACT : Outboard first wall total activity (Bq)
!  FWOGDR : Outboard first wall total gamma dose rate (Sv/hr)
!  FWOHKW : Outboard first wall total heat output (kW)
!  FWTEMP : Outboard first wall temperature after a LOCA (K)

  real(kind(1.0D0)) :: &
       bliizp,blimzp,bloizp,blomzp,fwiizp,fwimzp,fwoizp,fwomzp,fwtemp
  common /fisp1/ &
       bliizp,blimzp,bloizp,blomzp,fwiizp,fwimzp,fwoizp,fwomzp,fwtemp

  real(kind(1.0D0)), dimension(3) :: &
       bliact,bligdr,blihkw,bloact,blogdr,blohkw,fwiact,fwigdr, &
       fwihkw,fwoact,fwogdr,fwohkw
  common /fisp2/ &
       bliact,bligdr,blihkw,bloact,blogdr,blohkw,fwiact,fwigdr, &
       fwihkw,fwoact,fwogdr,fwohkw
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       bktlife,coolmass,cryomass,denstl,dewmkg,emult,fblbe,fblli2o, &
       fbllipb,fblli,fblss,fblvd,fhole,fvolbi,fvolbo,fvolcry,fvoldw, &
       fvolsi,fvolso,fwclfr,fwmass,pnucblkt,pnuccp,pnucloss,pnucshld, &
       ptfnuc,vdewex,vdewin,vfblkt,vfshld,volblkt,volblkti,volblkto, &
       volshld,whtblbe,whtblkt,wtblli2o,wtbllipb,whtblli,whtblss, &
       whtblvd,whtshld,wpenshld,wtshldi,wtshldo
  common /fwbsxx/ &
       bktlife,coolmass,cryomass,denstl,dewmkg,emult,fblbe,fblli2o, &
       fbllipb,fblli,fblss,fblvd,fhole,fvolbi,fvolbo,fvolcry,fvoldw, &
       fvolsi,fvolso,fwclfr,fwmass,pnucblkt,pnuccp,pnucloss,pnucshld, &
       ptfnuc,vdewex,vdewin,vfblkt,vfshld,volblkt,volblkti,volblkto, &
       volshld,whtblbe,whtblkt,wtblli2o,wtbllipb,whtblli,whtblss, &
       whtblvd,whtshld,wpenshld,wtshldi,wtshldo
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       baseel,crypmw,ctht,etahhten,etahhtex,etahlte,etahth,etath, &
       facht,fauxbop,fcsht,ffwlg,fgrosbop,fmgdmw,helecmw,hpower, &
       hthermmw,hvolume,helpow,htpmw,pacpmw,peakmva,pfwdiv,pgrossmw, &
       pinjht,pinjwp,pnetelmw,ppmphemw,priheat,psecht,pthermmw,pwpm2, &
       rnihx,rnphx,tfacpd,tlvpmw,trithtmw,vachtmw
  common /htpwr0/ &
       baseel,crypmw,ctht,etahhten,etahhtex,etahlte,etahth,etath, &
       facht,fauxbop,fcsht,ffwlg,fgrosbop,fmgdmw,helecmw,hpower, &
       hthermmw,hvolume,helpow,htpmw,pacpmw,peakmva,pfwdiv,pgrossmw, &
       pinjht,pinjwp,pnetelmw,ppmphemw,priheat,psecht,pthermmw,pwpm2, &
       rnihx,rnphx,tfacpd,tlvpmw,trithtmw,vachtmw

  integer :: ihplant,iprimhtp
  common /htpwr1/ ihplant,iprimhtp
!  $Id::                                                                $

  !  Main switches

  integer :: ife,ifetyp,ifedrv
  common /ifei/ ife,ifetyp,ifedrv

  !  Limits, f-values

  real(kind(1.0D0)) :: frrmax,rrmax
  common /ifelim/ frrmax,rrmax

  !  Physics

  real(kind(1.0D0)) :: &
       drveff,edrive,fburn,pdrive,tgain,gain,etadrv,reprat
  common /ifep1/ &
       drveff,edrive,fburn,pdrive,tgain,gain,etadrv,reprat

  real(kind(1.0D0)), dimension(10) :: etave,gainve
  common /ifep2/ etave,gainve

  !  Costs

  real(kind(1.0D0)) :: &
       uctarg,uccarb,ucconc,ucflib,cdriv0,cdriv1,cdriv2,dcdrv0, &
       dcdrv1,dcdrv2,mcdriv
  common /ifec1/ &
       uctarg,uccarb,ucconc,ucflib,cdriv0,cdriv1,cdriv2,dcdrv0, &
       dcdrv1,dcdrv2,mcdriv

  !  Device build and material fractions and masses

  integer, parameter ::  maxmat = 7

  real(kind(1.0D0)) :: &
       bldr,bldzl,bldzu,chrad,chdzl,chdzu,chvol,fwdr,fwdzl,fwdzu, &
       shdr,shdzl,shdzu,v1dr,v1dzl,v1dzu,v2dr,v2dzl,v2dzu,v3dr,v3dzl, &
       v3dzu,sombdr,somtdr,flirad,mflibe,fbreed
  common /ifeb1/ &
       bldr,bldzl,bldzu,chrad,chdzl,chdzu,chvol,fwdr,fwdzl,fwdzu, &
       shdr,shdzl,shdzu,v1dr,v1dzl,v1dzu,v2dr,v2dzl,v2dzu,v3dr,v3dzl, &
       v3dzu,sombdr,somtdr,flirad,mflibe,fbreed

  real(kind(1.0D0)), dimension(3,0:maxmat) :: &
       blmatf,blmatm,blmatv,fwmatf,fwmatm,fwmatv,shmatf,shmatm, &
       shmatv,v1matf,v1matm,v1matv,v2matf,v2matm,v2matv,v3matf, &
       v3matm,v3matv

  real(kind(1.0D0)), dimension(0:maxmat) :: chmatf,chmatm,chmatv

  real(kind(1.0D0)), dimension(3) :: blvol,fwvol,shvol,v1vol,v2vol,v3vol

  common /ifeb2/ &
     blmatf,blmatm,blmatv,blvol,chmatf,chmatm,chmatv,fwmatf,fwmatm, &
     fwmatv,fwvol,shmatf,shmatm,shmatv,shvol,v1matf,v1matm,v1matv, &
     v1vol,v2matf,v2matm,v2matv,v2vol,v3matf,v3matm,v3matv,v3vol

  real(kind(1.0D0)) :: &
       r1,r2,r3,r4,r5,r6,r7,zl1,zl2,zl3,zl4,zl5,zl6,zl7, &
       zu1,zu2,zu3,zu4,zu5,zu6,zu7
  common /ifeb3/ &
       r1,r2,r3,r4,r5,r6,r7,zl1,zl2,zl3,zl4,zl5,zl6,zl7, &
       zu1,zu2,zu3,zu4,zu5,zu6,zu7

  !  Heat transport

  real(kind(1.0D0)) :: pifecr,tdspmw,tfacmw,ptargf
  common /ifep1/ pifecr,tdspmw,tfacmw,ptargf
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       auxmin,betpmx,bmxlim,dtmpmx,fauxmn,fbeta,fbetap,fbetatry, &
       fdene,fdivcol,fdtmp,ffuspow,fgamcd,fhldiv,fiooic,fipir,fjohc, &
       fjohc0,fjprot,fjtfc,fmva,fpeakb,fpinj,fpnetel,fportsz,fptemp, &
       fq,fqval,frfpf,frfptf,frminor,fstrcase,fstrcond,ftburn,ftcycl, &
       ftmargtf,ftohs,ftpeak,fvdump,fvs,fwalld,gammax,mvalim,pnetelin, &
       powfmax,tbrnmn,tcycmn,tohsmn,tpkmax,walalw
  common /ineq/ &
       auxmin,betpmx,bmxlim,dtmpmx,fauxmn,fbeta,fbetap,fbetatry, &
       fdene,fdivcol,fdtmp,ffuspow,fgamcd,fhldiv,fiooic,fipir,fjohc, &
       fjohc0,fjprot,fjtfc,fmva,fpeakb,fpinj,fpnetel,fportsz,fptemp, &
       fq,fqval,frfpf,frfptf,frminor,fstrcase,fstrcond,ftburn,ftcycl, &
       ftmargtf,ftohs,ftpeak,fvdump,fvs,fwalld,gammax,mvalim,pnetelin, &
       powfmax,tbrnmn,tcycmn,tohsmn,tpkmax,walalw
!  $Id::                                                                $

  !  ngrpmx is the maximum number of PF coil groups
  !  nclsmx is the maximum number of coils in one group
  !  nptsmx is the maximum number of points across the plasma midplane
  !         at which the magnetic field is fixed
  !  nfixmx is the maximum number of fixed current coils

  integer, parameter :: ngrpmx = 8
  integer, parameter :: nclsmx = 2
  integer, parameter :: nptsmx = 32
  integer, parameter :: nfixmx = 64
  integer, parameter :: ngc = ngrpmx*nclsmx
  integer, parameter :: ngc2 = ngc+2

  real(kind(1.0D0)) :: &
       acsoh,ac1oh,alfapf,bmaxoh,bmaxoh0,cohbof,cohbop,coheof,cptoh, &
       fcohbof,fcohbop,fcuoh,ohhghf,pfclres,powohres,powpfres,rjohc, &
       rjohc0,rohc,rpf1,rpf2,sccufac,sigpfalw,vfohc,whtpf,whtpfs
  common /pfc0/ &
       acsoh,ac1oh,alfapf,bmaxoh,bmaxoh0,cohbof,cohbop,coheof,cptoh, &
       fcohbof,fcohbop,fcuoh,ohhghf,pfclres,powohres,powpfres,rjohc, &
       rjohc0,rohc,rpf1,rpf2,sccufac,sigpfalw,vfohc,whtpf,whtpfs

  integer ::ipfres,isumatpf,ncirt,ngrp,nohc
  common /pfc1/ ipfres,isumatpf,ncirt,ngrp,nohc

  real(kind(1.0D0)), dimension(ngc2) :: &
       bpf,cptdin,curpfb,curpff,curpfs,ra,rb,ric,rjconpf,rjpfalw, &
       rpf,turns,vf,wtc,wts,zh,zl,zpf
  real(kind(1.0D0)), dimension(ngc2,6) :: cpt,waves
  common /pfc2/ &
       bpf,cpt,cptdin,curpfb,curpff,curpfs,ra,rb,ric,rjconpf,rjpfalw, &
       rpf,turns,vf,waves,wtc,wts,zh,zl,zpf

  integer, dimension(ngc) :: ipfloc
  integer, dimension(ngrpmx+2) :: ncls
  common /pfc3/ ipfloc,ncls

  !  PF scaling variables :

  integer :: nfxfh
  common /pfscl1/ nfxfh

  real(kind(1.0D0)) :: routr
  common /pfscl2/ routr

  real(kind(1.0D0)), dimension(ngrpmx) :: zref
  common /pfscl3/ zref
!  $Id::                                                                $

  integer, parameter :: ipnlaws = 36  !  number of energy confinement time scaling laws

  character(len=24), dimension(ipnlaws) :: tauscl
  common /label2/ tauscl

  real(kind(1.0D0)) :: &
       abeam,afuel,aion,alphaj,alphan,alphat,alpmw,aspect,beamfus0, &
       beta,betaft,betalim,betanb,betap,betbm0,bp,bt,btot,burnup,capa, &
       carea,cfe0,csawth,cvol,deltdn,deltup,dene,deni,dign,dlamee, &
       dlamie,dnalp,dnbeam,dnbeam2,dnbeta,dnelimt,dnitot,dnla,dnprot, &
       dntau,dnz,ealpha,epbetmax,eps,faccd,facoh,falpe,falpha,falpi, &
       faux,fbfe,fdeut,ffwal,fhe3,figmer,fradmin,ftr,ftrit,fvsbrnni, &
       gamma,hfact,impc,impfe,impo,kappa,kappa95,kappaa,palp,palpe, &
       palpi,palpnb,pbrem,pcharge,pcoef
  common /phydt0/ &
       abeam,afuel,aion,alphaj,alphan,alphat,alpmw,aspect,beamfus0, &
       beta,betaft,betalim,betanb,betap,betbm0,bp,bt,btot,burnup,capa, &
       carea,cfe0,csawth,cvol,deltdn,deltup,dene,deni,dign,dlamee, &
       dlamie,dnalp,dnbeam,dnbeam2,dnbeta,dnelimt,dnitot,dnla,dnprot, &
       dntau,dnz,ealpha,epbetmax,eps,faccd,facoh,falpe,falpha,falpi, &
       faux,fbfe,fdeut,ffwal,fhe3,figmer,fradmin,ftr,ftrit,fvsbrnni, &
       gamma,hfact,impc,impfe,impo,kappa,kappa95,kappaa,palp,palpe, &
       palpi,palpnb,pbrem,pcharge,pcoef

  real(kind(1.0D0)) :: &
       pdivt,pfuscmw,phiint,pi,pie,plascur,plrad,pneut,pohmpv, &
       powerht,powfmw,prad,protonmw,psync,ptre,ptri,q,q0,q95,qfuel, &
       qlim,qstar,ralpne,recyle,rli,rlp,rmajor,rminor,rmu0,rnbeam, &
       rncne,rndfuel,rnfene,rnone,rpfac,rplas,rtpte,sarea,sareao,sf, &
       ssync,tauee,taueff,tauei,te,ten,ti,tin,tratio,triang,triang95, &
       vol,vsbrn,vshift,vsind,vsres,vsstt,wallmw,wtgpd,xarea,zeff,zeffai
  common /phydt1/ &
       pdivt,pfuscmw,phiint,pi,pie,plascur,plrad,pneut,pohmpv, &
       powerht,powfmw,prad,protonmw,psync,ptre,ptri,q,q0,q95,qfuel, &
       qlim,qstar,ralpne,recyle,rli,rlp,rmajor,rminor,rmu0,rnbeam, &
       rncne,rndfuel,rnfene,rnone,rpfac,rplas,rtpte,sarea,sareao,sf, &
       ssync,tauee,taueff,tauei,te,ten,ti,tin,tratio,triang,triang95, &
       vol,vsbrn,vshift,vsind,vsres,vsstt,wallmw,wtgpd,xarea,zeff,zeffai

  real(kind(1.0D0)), dimension(7) :: dlimit
  real(kind(1.0D0)), dimension(ipnlaws) :: hfac
  real(kind(1.0D0)), dimension(5) :: pthrmw
  common /phydt2/ dlimit,hfac,pthrmw

  integer :: &
       gtscale,ibss,iculbl,iculdl,icurr,idensl,idhe3,idivrt, &
       ifispact,igeom,iinvqd,iiter,ires,isc,iscrp,ishape,itart,ignite, &
       iwalld,ifalphap
  common /phydt3/ &
       gtscale,ibss,iculbl,iculdl,icurr,idensl,idhe3,idivrt, &
       ifispact,igeom,iinvqd,iiter,ires,isc,iscrp,ishape,itart,ignite, &
       iwalld,ifalphap
!  $Id::                                                                $

!--Version number 1.100
!
!--Description
!  Include file containing pulsed reactor variables
!
!--Author
!  Chris Gardner, c/o
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  11 April 1994
!
!--Reference
!  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
!  
!--History
!  08/11/93 PJK 1.000 Initial version
!  11/04/94 PJK 1.100 Changed ITPULS to ITCYCL
!
!--Contents
!  afw    : inner radius of each first wall structural cylinder (m)
!  bfw    : outer radius of each first wall structural cylinder (m)
!  bctmp  : bulk coolant temperature (C)
!  coolp  : coolant pressure (Pa)
!  dtstor : maximum allowable temperature change within the stainless
!           steel thermal storage block (K)
!  fwlife : first wall lifetime (yrs)
!  tmprse : temperature rise in coolant along toroidal
!           extent of first wall (C)
!  tpeak  : peak temperature in first wall (C)
!  istore : switch for thermal storage method (1/2/3)
!  itcycl : switch for first wall axial stress model (1/2/3)
!  lpulse : switch for reactor model : 1 = pulsed, 0 = continuous

  real(kind(1.0D0)) :: &
       afw,bfw,bctmp,coolp,dtstor,fwlife,tmprse,tpeak
  common /pulse1/ &
       afw,bfw,bctmp,coolp,dtstor,fwlife,tmprse,tpeak

  integer ::istore,itcycl,lpulse
  common /pulse2/ istore,itcycl,lpulse
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       acptmax,ensxpfm,pfckts,spfbusl,spsmva,srcktpm,vpfskv
  common /pwrcom/ &
       acptmax,ensxpfm,pfckts,spfbusl,spsmva,srcktpm,vpfskv
!  $Id::                                                                $

!--Version number 1.000
!
!--Description
!  INCLUDE file for reversed-field pinch module in PROCESS.
!
!--Author
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  27 February 1996
!
!--Reference
!  None
!  
!--History
!  27/02/96 PJK 1.000 Initial version
!
!--Contents
!  irfp   : Switch for rfp option (0=off)
!  nrfppf : number of RFP PF coils
!  rrpf   : radius of each RFP PF coil (m)
!  zzpf   : vertical position of each RFP PF coil (m)
!  drpf   : radial cross-section of each RFP PF coil (m)
!  dzpf   : vertical cross-section of each RFP PF coil (m)
!  nturns : number of turns of each RFP PF coil
!  cptrfp : current per turn in each RFP PF coil (A/m2)
!  resrfp : resistance of each RFP PF coil
!  tftort : TF coil toroidal thickness (m)
!  pfrmax : radius of largest PF coil (m)
!  pfmmax : mass of heaviest PF coil (tonnes)
!  rfpf   : reversal parameter F
!  rfpth  : pinch parameter theta

  integer, parameter :: nrfppf = 16

  real(kind(1.0D0)), dimension(nrfppf) :: &
       rrpf,zzpf,drpf,dzpf,nturns,cptrfp,resrfp
  common /rfpdar/ &
       rrpf,zzpf,drpf,dzpf,nturns,cptrfp,resrfp

  real(kind(1.0D0)) :: tftort,pfrmax,pfmmax,rfpf,rfpth
  common /rfpdbl/ tftort,pfrmax,pfmmax,rfpf,rfpth

  integer :: irfp
  common /rfpint/ irfp
!  $Id::                                                                $

!--Version number 1.000
!
!--Description
!  INCLUDE file for plasma start-up routine
!
!--Author
!  Peter Knight D3/012 Culham Laboratory, ext.3330
!
!--Date
!  08 November 1993
!
!--Reference
!  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
!  
!--History
!  08/11/93 PJK 1.000 Initial version
!
!--Contents
!  nign   : electron density at start-up (m**-3)
!  tign   : electron temperature at start-up (keV)
!  ptaue  : exponent in taue formula
!  qtaue  : exponent in taue formula
!  rtaue  : exponent in taue formula
!  gtaue  : factor in taue formula
!  ftaue  : factor in taue formula
!  aa     : constant
!  bb     : constant
!  cc     : constant
!  dd     : constant
!  s      : constant

  real(kind(1.0D0)) :: &
       nign,tign,ptaue,qtaue,rtaue,gtaue,ftaue,aa,bb,cc,dd
  common /strt1/ &
       nign,tign,ptaue,qtaue,rtaue,gtaue,ftaue,aa,bb,cc,dd

  integer :: s
  common /strt2/ s
!  $Id::                                                                $

!--Version number 1.000
!
!--Description
!  INCLUDE file for stellarator module in PROCESS.
!
!--Author
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  28 June 1994
!
!--Reference
!  None
!  
!--History
!  28/06/94 PJK 1.000 Initial version
!
!--Contents
!  istell : Switch for stellarator option (0=off)
!  isthtr : Switch for different auxiliary heating methods

  integer :: istell,isthtr
  common /stlint/ istell,isthtr
!  $Id::                                                                $

  real(kind(1.0D0)) :: aintmass,clgsmass,coldmass,fncmass,gsmass
  common /struc1/ aintmass,clgsmass,coldmass,fncmass,gsmass
!  $Id::                                                                $

  integer :: itfsup
  common /tfcom0/ itfsup

  !  Resistive TF coil variables

  real(kind(1.0D0)) :: &
       arealeg,bmaxtf,cdtfleg,cforce,cpres,cpttf,drtop,dztop,estotf, &
       fcoolcp,jbus,oacdcp,prescp,rbmax,rhocp,rhotfleg,ripmax,ripple, &
       ritfc,sigrad,sigtan,sigver,tcpav,tfareain,tfboreh,tfbusl, &
       tfbusmas,tfcmw,tfcpmw,tflegmw,tflegres,tfno,tmpcry,turnstf, &
       vforce,vftf,volcp,voltfleg,vtfkv,whtcp,whttf,whttflgs,wpvf,wtbc
  common /tfcom1/ &
       arealeg,bmaxtf,cdtfleg,cforce,cpres,cpttf,drtop,dztop,estotf, &
       fcoolcp,jbus,oacdcp,prescp,rbmax,rhocp,rhotfleg,ripmax,ripple, &
       ritfc,sigrad,sigtan,sigver,tcpav,tfareain,tfboreh,tfbusl, &
       tfbusmas,tfcmw,tfcpmw,tflegmw,tflegres,tfno,tmpcry,turnstf, &
       vforce,vftf,volcp,voltfleg,vtfkv,whtcp,whttf,whttflgs,wpvf,wtbc

  !  Centrepost variables

  real(kind(1.0D0)) :: &
       cph2o,denh2o,etapump,frhocp,kcp,kh2o,muh2o,ncool,ppump, &
       ptempalw,rcool,tcoolin,tcpav2,tcpmax,vcool
  common /tfcom2/ &
       cph2o,denh2o,etapump,frhocp,kcp,kh2o,muh2o,ncool,ppump, &
       ptempalw,rcool,tcoolin,tcpav2,tcpmax,vcool

  !  Superconducting TF coil variables

  integer :: isumattf,itfmod,magnt,jcrit_model
  common /tfcom3/ isumattf,itfmod,magnt,jcrit_model

  real(kind(1.0D0)) :: &
       acasetf,acndttf,acond,acstf,aiwp,alstrtf,aspcstf,aswp,avwp, &
       bcritsc,bmaxtfrp,borev,casestr,casfact,casthi,casths,csutf, &
       csytf,dcase,dcopper,deflect,eyins,eystl,eywp,fcutfsu,jcritsc, &
       jwdgcrt,jwdgpro,jwptf,poisson,rjtfsual,rnltf,sigrcon,sigtcon, &
       sigvert,strncon,strtf1,strtf2,tcritsc,tdmptf,tfckw,tficrn, &
       tfind,tfleng,tfocrn,tfsai,tfsao,tftmp,thicndut,thkcas,thkwp, &
       thwcndut,tinstf,tmargmin,tmargtf,tmaxpro,vdalw,vtfskv,whtcas, &
       whtcon,whtconcu,whtconsc,whtconsh,wwp1,wwp2
  common /tfcom4/ &
       acasetf,acndttf,acond,acstf,aiwp,alstrtf,aspcstf,aswp,avwp, &
       bcritsc,bmaxtfrp,borev,casestr,casfact,casthi,casths,csutf, &
       csytf,dcase,dcopper,deflect,eyins,eystl,eywp,fcutfsu,jcritsc, &
       jwdgcrt,jwdgpro,jwptf,poisson,rjtfsual,rnltf,sigrcon,sigtcon, &
       sigvert,strncon,strtf1,strtf2,tcritsc,tdmptf,tfckw,tficrn, &
       tfind,tfleng,tfocrn,tfsai,tfsao,tftmp,thicndut,thkcas,thkwp, &
       thwcndut,tinstf,tmargmin,tmargtf,tmaxpro,vdalw,vtfskv,whtcas, &
       whtcon,whtconcu,whtconsc,whtconsh,wwp1,wwp2

  real(kind(1.0D0)), dimension(5) :: dcond,eyoung,jeff,sigrtf,sigttf
  real(kind(1.0D0)), dimension(6) :: radtf
  common /tfcom5/ dcond,eyoung,jeff,radtf,sigrtf,sigttf

  real(kind(1.0D0)) :: farc4tf
  common /tfcom6/ farc4tf

  real(kind(1.0D0)), dimension(5) :: dthet,radctf,xarc,xctfc,yarc,yctfc
  common /tfcom7/ dthet,radctf,xarc,xctfc,yarc,yctfc    
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       tburn,tburn0,tdown,tdwell,theat,tohs,tohsin,tpulse,tqnch,tramp
  common /times0/ &
       tburn,tburn0,tdown,tdwell,theat,tohs,tohsin,tpulse,tqnch,tramp

  real(kind(1.0D0)), dimension(6) :: tim
  common /times1/ tim
!  $Id::                                                                $

  real(kind(1.0D0)) :: dlscal,vacdshm,vcdimax,vpumpn
  common /tors0/ dlscal,vacdshm,vcdimax,vpumpn

  integer :: nvduct,nvtype
  common /tors1/ nvduct,nvtype
!  $Id::                                                                $

  real(kind(1.0D0)) :: pbase,prdiv,rat,tn
  common /vac0/ pbase,prdiv,rat,tn

  integer :: ntype
  common /vac1/ ntype
!  $Id::                                                                $

  real(kind(1.0D0)) :: &
       vsbn,vsefbn,vsefsu,vseft,vsoh,vsohbn,vsohsu,vssu,vstot
  common /vltcm0/ &
       vsbn,vsefbn,vsefsu,vseft,vsoh,vsohbn,vsohsu,vssu,vstot

  real(kind(1.0D0)), dimension(ngc2,ngc2) :: sxlg
  common /vltcm1/ sxlg
end module wibble
