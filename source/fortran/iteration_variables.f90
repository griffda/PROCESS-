module define_iteration_variables
   use build_variables
   use constraint_variables
   use cost_variables
   use current_drive_variables
   use divertor_kallenbach_variables
   use divertor_variables
   use error_handling
   use fwbs_variables
   use heat_transport_variables
   use impurity_radiation_module
   use numerics
   use pfcoil_variables
   use physics_variables
   use plasmod_variables
   use rebco_variables
   use reinke_variables
   use tfcoil_variables
   use times_variables
   implicit none; public
   real(kind(1.0d0))::DUMMY
   contains
   !+ad_vars  lablxc(ipnvars) : labels describing iteration variables<UL> 
   
   !---------------------------------
   subroutine init_itv_1
      !+ad_varc  <LI> ( 1) aspect
      lablxc(1) = 'aspect        '
      boundl(1) = 1.100D0
      boundu(1) = 10.00D0 
   end subroutine

   real(kind(1.d0)) function itv_1(); itv_1 = aspect; end 
   subroutine set_itv_1(ratio)
      real(kind(1.d0))::ratio
      aspect = ratio
   end subroutine
   !---------------------------------
   subroutine init_itv_2
      !+ad_varc  <LI> ( 2) bt
      lablxc(2) = 'bt            '
      boundl(2) = 0.010D0 
      boundu(2) = 30.00D0
   end subroutine

   real(kind(1.d0)) function itv_2(); itv_2 = bt; end 
   subroutine set_itv_2(ratio)
      real(kind(1.d0))::ratio
      bt = ratio
   end subroutine
  !---------------------------------
  subroutine init_itv_3
       !+ad_varc  <LI> ( 3) rmajor
       lablxc(3) = 'rmajor        '
      boundl(3) = 0.100D0 
      boundu(3) = 50.00D0  
  end subroutine

  real(kind(1.d0)) function itv_3(); itv_3 = rmajor; end

  subroutine set_itv_3(ratio)
    real(kind(1.d0))::ratio
    rmajor = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_4
       !+ad_varc  <LI> ( 4) te
       lablxc(4) = 'te            '
      boundl(4) = 5.000D0 
      boundu(4) = 150.0D0  
  end subroutine

  real(kind(1.d0)) function itv_4(); itv_4 = te; end

  subroutine set_itv_4(ratio)
    real(kind(1.d0))::ratio
    te = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_5
       !+ad_varc  <LI> ( 5) beta
       lablxc(5) = 'beta          '
      boundl(5) = 0.001D0 
      boundu(5) = 1.000D0  
  end subroutine

  real(kind(1.d0)) function itv_5(); itv_5 = beta; end

  subroutine set_itv_5(ratio)
    real(kind(1.d0))::ratio
    beta = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_6
       !+ad_varc  <LI> ( 6) dene
       lablxc(6) = 'dene          '
      boundl(6) = 1.00D19 
      boundu(6) = 1.00D21 
  end subroutine

  real(kind(1.d0)) function itv_6(); itv_6 = dene; end

  subroutine set_itv_6(ratio)
    real(kind(1.d0))::ratio
    dene = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_7
       !+ad_varc  <LI> ( 7) rnbeam
       lablxc(7) = 'rnbeam        '
      boundl(7) = 1.00D-6 
      boundu(7) = 1.000D0
  end subroutine

  real(kind(1.d0)) function itv_7(); itv_7 = rnbeam; end

  subroutine set_itv_7(ratio)
    real(kind(1.d0))::ratio
    rnbeam = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_8
       !+ad_varc  <LI> ( 8) fbeta (f-value for equation 6)
       lablxc(8) = 'fbeta         '
      boundl(8) = 0.001D0 
      boundu(8) = 1.000D0  
  end subroutine

  real(kind(1.d0)) function itv_8(); itv_8 = fbeta; end

  subroutine set_itv_8(ratio)
    real(kind(1.d0))::ratio
    fbeta = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_9  
       !+ad_varc  <LI> ( 9) fdene (f-value for equation 5)
       lablxc(9) = 'fdene         '
      boundl(9) =  0.001D0 
      boundu(9) = 1.000D0
  end subroutine

  real(kind(1.d0)) function itv_9(); itv_9 = fdene; end

  subroutine set_itv_9(ratio)
    real(kind(1.d0))::ratio
    fdene = ratio
  end subroutine

!---------------------------------
subroutine init_itv_10
       !+ad_varc  <LI> (10) hfact
       lablxc(10) = 'hfact         '
      boundl(10) = 0.100D0 
      boundu(10) = 3.000D0  
end subroutine

real(kind(1.d0)) function itv_10()
   itv_10 =  hfact
end function

subroutine set_itv_10(ratio)
  real(kind(1.d0))::ratio
   hfact = ratio
end subroutine
!---------------------------------
subroutine init_itv_11 
       !+ad_varc  <LI> (11) pheat
       lablxc(11) = 'pheat         '
      boundl(11) = 1.00D-3 
      boundu(11) = 1.000D3
end subroutine

real(kind(1.d0)) function itv_11()
   itv_11 =  pheat
end function

subroutine set_itv_11(ratio)
  real(kind(1.d0))::ratio
   pheat = ratio
end subroutine
!---------------------------------
subroutine init_itv_12
       !+ad_varc  <LI> (12) oacdcp
       lablxc(12) = 'oacdcp        '
      boundl(12) = 1.000D5 
      boundu(12) =  1.500D8 
end subroutine

real(kind(1.d0)) function itv_12()
   itv_12 =  oacdcp
end function

subroutine set_itv_12(ratio)
  real(kind(1.d0))::ratio
   oacdcp = ratio
end subroutine
!---------------------------------
subroutine init_itv_13
       !+ad_varc  <LI> (13) tfcth (NOT RECOMMENDED)
       lablxc(13) = 'tfcth         '
      boundl(13) = 0.100D0 
      boundu(13) = 5.000D0
end subroutine

real(kind(1.d0)) function itv_13()
   itv_13 =  tfcth
end function

subroutine set_itv_13(ratio)
  real(kind(1.d0))::ratio
   tfcth = ratio
end subroutine
!---------------------------------
subroutine init_itv_14
       !+ad_varc  <LI> (14) fwalld (f-value for equation 8)
       lablxc(14) = 'fwalld        '
      boundl(14) = 0.001D0 
      boundu(14) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_14()
   itv_14 =  fwalld
end function

subroutine set_itv_14(ratio)
  real(kind(1.d0))::ratio
   fwalld = ratio
end subroutine
!---------------------------------
subroutine init_itv_15
       !+ad_varc  <LI> (15) fvs (f-value for equation 12)
       lablxc(15) = 'fvs           '
      boundl(15) = 0.001D0 
      boundu(15) = 1.000
end subroutine

real(kind(1.d0)) function itv_15()
   itv_15 =  fvs
end function

subroutine set_itv_15(ratio)
  real(kind(1.d0))::ratio
   fvs = ratio
end subroutine
!---------------------------------
subroutine init_itv_16
       !+ad_varc  <LI> (16) ohcth
       lablxc(16) = 'ohcth         '
      boundl(16) = 0.010D0 
      boundu(16) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_16()
   itv_16 =  ohcth
end function

subroutine set_itv_16(ratio)
  real(kind(1.d0))::ratio
   ohcth = ratio
end subroutine
!---------------------------------
subroutine init_itv_17
       !+ad_varc  <LI> (17) tdwell
       lablxc(17) = 'tdwell        '
      boundl(17) = 0.100D0 
      boundu(17) = 1.000D8
end subroutine

real(kind(1.d0)) function itv_17()
   itv_17 =  tdwell
end function

subroutine set_itv_17(ratio)
  real(kind(1.d0))::ratio
   tdwell = ratio
end subroutine
!---------------------------------
subroutine init_itv_18
       !+ad_varc  <LI> (18) q
       lablxc(18) = 'q             '
      boundl(18) = 2.000D0 
      boundu(18) = 50.00D0
end subroutine

real(kind(1.d0)) function itv_18()
   itv_18 =  q
end function

subroutine set_itv_18(ratio)
  real(kind(1.d0))::ratio
   q = ratio
end subroutine
!---------------------------------
subroutine init_itv_19
       !+ad_varc  <LI> (19) enbeam
       lablxc(19) = 'enbeam        '
      boundl(19) = 1.000D0 
      boundu(19) = 1.000D6
end subroutine

real(kind(1.d0)) function itv_19()
   itv_19 =  enbeam
end function

subroutine set_itv_19(ratio)
  real(kind(1.d0))::ratio
  enbeam = ratio
end subroutine
!---------------------------------
subroutine init_itv_20
       !+ad_varc  <LI> (20) tcpav
       lablxc(20) = 'tcpav         '
      boundl(20) = 40.00D0 
      boundu(20) = 3.000D2
end subroutine

real(kind(1.d0)) function itv_20()
   itv_20 = tcpav 
end function

subroutine set_itv_20(ratio)
  real(kind(1.d0))::ratio
  tcpav = ratio
end subroutine
!---------------------------------
subroutine init_itv_21
       !+ad_varc  <LI> (21) ftburn (f-value for equation 13)
       lablxc(21) = 'ftburn        '
      boundl(21) = 0.001D0 
      boundu(21) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_21()
   itv_21 =  ftburn
end function

subroutine set_itv_21(ratio)
  real(kind(1.d0))::ratio
  ftburn = ratio
end subroutine
       !+ad_varc  <LI> (22) NOT USED
!---------------------------------
subroutine init_itv_23
       !+ad_varc  <LI> (23) fcoolcp
       lablxc(23) = 'fcoolcp       '
      boundl(23) = 0.100D0 
      boundu(23) = 0.500D0
end subroutine

real(kind(1.d0)) function itv_23()
   itv_23 =  fcoolcp
end function

subroutine set_itv_23(ratio)
  real(kind(1.d0))::ratio
  fcoolcp = ratio
end subroutine
!---------------------------------
       !+ad_varc  <LI> (24) NOT USED
!---------------------------------
subroutine init_itv_25
       !+ad_varc  <LI> (25) fpnetel (f-value for equation 16)
       lablxc(25) = 'fpnetel       '
      boundl(25) = 0.001D0
      boundu(25) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_25()
   itv_25 =  fpnetel
end function

subroutine set_itv_25(ratio)
  real(kind(1.d0))::ratio
  fpnetel = ratio
end subroutine
!---------------------------------
subroutine init_itv_26
       !+ad_varc  <LI> (26) ffuspow (f-value for equation 9)
       lablxc(26) = 'ffuspow       '
      boundl(26) = 0.001D0
      boundu(26) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_26()
   itv_26 =  ffuspow
end function

subroutine set_itv_26(ratio)
  real(kind(1.d0))::ratio
  ffuspow = ratio
end subroutine
!---------------------------------
subroutine init_itv_27
       !+ad_varc  <LI> (27) fhldiv (f-value for equation 18)
       lablxc(27) = 'fhldiv        ';  boundl(27) = 0.001D0
      boundu(27) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_27()
   itv_27 =  fhldiv
end function

subroutine set_itv_27(ratio)
  real(kind(1.d0))::ratio
  fhldiv = ratio
end subroutine
!---------------------------------
subroutine init_itv_28
       !+ad_varc  <LI> (28) fradpwr (f-value for equation 17), total radiation fraction
       lablxc(28) = 'fradpwr       '
      boundl(28) = 0.001D0
      boundu(28) = 0.990D0
end subroutine

real(kind(1.d0)) function itv_28()
   itv_28 =  fradpwr
end function

subroutine set_itv_28(ratio)
  real(kind(1.d0))::ratio
  fradpwr = ratio
end subroutine
!---------------------------------
subroutine init_itv_29
       !+ad_varc  <LI> (29) bore
       lablxc(29) = 'bore          '
      boundl(29) = 0.100D0
      boundu(29) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_29()
   itv_29 =  bore
end function

subroutine set_itv_29(ratio)
  real(kind(1.d0))::ratio
  bore = ratio
end subroutine
!---------------------------------
subroutine init_itv_30
       !+ad_varc  <LI> (30) fmva (f-value for equation 19)
       lablxc(30) = 'fmva          '
      boundl(30) = 0.010D0
      boundu(30) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_30()
   itv_30 =  fmva
end function

subroutine set_itv_30(ratio)
  real(kind(1.d0))::ratio
  fmva = ratio
end subroutine
!---------------------------------
subroutine init_itv_31
       !+ad_varc  <LI> (31) gapomin
       lablxc(31) = 'gapomin       '
      boundl(31) = 0.001D0
      boundu(31) = 1.000D1
end subroutine

real(kind(1.d0)) function itv_31()
   itv_31 =  gapomin
end function

subroutine set_itv_31(ratio)
  real(kind(1.d0))::ratio
  gapomin = ratio
end subroutine
!---------------------------------
subroutine init_itv_32
       !+ad_varc  <LI> (32) frminor (f-value for equation 21)
       lablxc(32) = 'frminor       '
      boundl(32) = 0.001D0
      boundu(32) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_32()
   itv_32 =  frminor
end function

subroutine set_itv_32(ratio)
  real(kind(1.d0))::ratio
  frminor = ratio
end subroutine
!---------------------------------
subroutine init_itv_33
       !+ad_varc  <LI> (33) fportsz (f-value for equation 20)
       lablxc(33) = 'fportsz       '
      boundl(33) = 0.001D0
      boundu(33) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_33()
   itv_33 =  fportsz
end function

subroutine set_itv_33(ratio)
  real(kind(1.d0))::ratio
  fportsz = ratio
end subroutine
!---------------------------------
subroutine init_itv_34
       !+ad_varc  <LI> (34) fdivcol (f-value for equation 22)
       lablxc(34) = 'fdivcol       '
      boundl(34) = 0.001D0
      boundu(34) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_34()
   itv_34 =  fdivcol
end function

subroutine set_itv_34(ratio)
  real(kind(1.d0))::ratio
  fdivcol = ratio
end subroutine
!---------------------------------
subroutine init_itv_35
       !+ad_varc  <LI> (35) fpeakb (f-value for equation 25)
       lablxc(35) = 'fpeakb        '
      boundl(35) = 0.001D0
      boundu(35) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_35()
   itv_35 =  fpeakb
end function

subroutine set_itv_35(ratio)
  real(kind(1.d0))::ratio
  fpeakb = ratio
end subroutine
!---------------------------------
subroutine init_itv_36
       !+ad_varc  <LI> (36) fbetatry (f-value for equation 24)
       lablxc(36) = 'fbetatry      '
      boundl(36) = 0.001D0
      boundu(36) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_36()
   itv_36 =  fbetatry
end function

subroutine set_itv_36(ratio)
  real(kind(1.d0))::ratio
  fbetatry = ratio
end subroutine
!---------------------------------
subroutine init_itv_37
       !+ad_varc  <LI> (37) coheof
       lablxc(37) = 'coheof        '
      boundl(37) = 1.000D5
      boundu(37) = 1.000D8
end subroutine

real(kind(1.d0)) function itv_37()
   itv_37 =  coheof
end function

subroutine set_itv_37(ratio)
  real(kind(1.d0))::ratio
  coheof = ratio
end subroutine
!---------------------------------
subroutine init_itv_38
       !+ad_varc  <LI> (38) fjohc (f-value for equation 26)
       lablxc(38) = 'fjohc         '
      boundl(38) = 0.010D0
      boundu(38) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_38()
   itv_38 =  fjohc
end function

subroutine set_itv_38(ratio)
  real(kind(1.d0))::ratio
  fjohc = ratio
end subroutine
!---------------------------------
subroutine init_itv_39
       !+ad_varc  <LI> (39) fjohc0 (f-value for equation 27)
       lablxc(39) = 'fjohc0        '
      boundl(39) = 0.001D0
      boundu(39) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_39()
   itv_39 =  fjohc0
end function

subroutine set_itv_39(ratio)
  real(kind(1.d0))::ratio
  fjohc0 = ratio
end subroutine
!---------------------------------
subroutine init_itv_40
       !+ad_varc  <LI> (40) fgamcd (f-value for equation 37)
       lablxc(40) = 'fgamcd        '
      boundl(40) = 0.001D0
      boundu(40) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_40()
   itv_40 =  fgamcd
end function

subroutine set_itv_40(ratio)
  real(kind(1.d0))::ratio
  fgamcd = ratio
end subroutine
!---------------------------------
subroutine init_itv_41
       !+ad_varc  <LI> (41) fcohbop
       lablxc(41) = 'fcohbop       '
      boundl(41) = 0.001D0
      boundu(41) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_41()
   itv_41 =  fcohbop
end function

subroutine set_itv_41(ratio)
  real(kind(1.d0))::ratio
  fcohbop = ratio
end subroutine
!---------------------------------
subroutine init_itv_42
       !+ad_varc  <LI> (42) gapoh
       lablxc(42) = 'gapoh         '
      boundl(42) = 0.001D0
      boundu(42) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_42()
   itv_42 =  gapoh
end function

subroutine set_itv_42(ratio)
  real(kind(1.d0))::ratio
  gapoh = ratio
end subroutine
!---------------------------------
! 43 not used
!---------------------------------
subroutine init_itv_44
       !+ad_varc  <LI> (44) fvsbrnni
       lablxc(44) = 'fvsbrnni      '
      boundl(44) = 0.001D0
      boundu(44) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_44()
   itv_44 =  fvsbrnni
end function

subroutine set_itv_44(ratio)
  real(kind(1.d0))::ratio
 fvsbrnni  = ratio
end subroutine
!---------------------------------
subroutine init_itv_45
       !+ad_varc  <LI> (45) fqval (f-value for equation 28)
       lablxc(45) = 'fqval         '
      boundl(45) = 0.001D0
      boundu(45) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_45()
   itv_45 =  fqval
end function

subroutine set_itv_45(ratio)
  real(kind(1.d0))::ratio
  fqval = ratio
end subroutine
!---------------------------------
subroutine init_itv_46
       !+ad_varc  <LI> (46) fpinj (f-value for equation 30)
       lablxc(46) = 'fpinj         '
      boundl(46) = 0.001D0
      boundu(46) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_46()
   itv_46 =  fpinj
end function

subroutine set_itv_46(ratio)
  real(kind(1.d0))::ratio
  fpinj = ratio
end subroutine
!---------------------------------
subroutine init_itv_47
       !+ad_varc  <LI> (47) feffcd
       lablxc(47) = 'feffcd        '
      boundl(47) = 0.001D0
      boundu(47) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_47()
   itv_47 = feffcd 
end function

subroutine set_itv_47(ratio)
  real(kind(1.d0))::ratio
  feffcd = ratio
end subroutine
!---------------------------------
subroutine init_itv_48
       !+ad_varc  <LI> (48) fstrcase (f-value for equation 31)
       lablxc(48) = 'fstrcase      '
      boundl(48) = 0.001D0
      boundu(48) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_48()
   itv_48 =  fstrcase
end function

subroutine set_itv_48(ratio)
  real(kind(1.d0))::ratio
  fstrcase = ratio
end subroutine
!---------------------------------
subroutine init_itv_49
       !+ad_varc  <LI> (49) fstrcond (f-value for equation 32)
       lablxc(49) = 'fstrcond      '
      boundl(49) = 0.001D0
      boundu(49) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_49()
   itv_49 =  fstrcond
end function

subroutine set_itv_49(ratio)
  real(kind(1.d0))::ratio
  fstrcond = ratio
end subroutine
!---------------------------------
subroutine init_itv_50
       !+ad_varc  <LI> (50) fiooic (f-value for equation 33)
       lablxc(50) = 'fiooic        '
      boundl(50) = 0.001D0
      boundu(50) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_50()
   itv_50 =  fiooic
end function

subroutine set_itv_50(ratio)
  real(kind(1.d0))::ratio
  fiooic = ratio
end subroutine
!---------------------------------
subroutine init_itv_51
       !+ad_varc  <LI> (51) fvdump (f-value for equation 34)
       lablxc(51) = 'fvdump        '
      boundl(51) = 0.001D0
      boundu(51) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_51()
   itv_51 =  fvdump
end function

subroutine set_itv_51(ratio)
  real(kind(1.d0))::ratio
  fvdump = ratio
end subroutine
!---------------------------------
subroutine init_itv_52
       !+ad_varc  <LI> (52) vdalw
       lablxc(52) = 'vdalw         '
      boundl(52) = 0.001D0
      boundu(52) = 1.000D6
end subroutine

real(kind(1.d0)) function itv_52()
   itv_52 =  vdalw
end function

subroutine set_itv_52(ratio)
  real(kind(1.d0))::ratio
  vdalw = ratio
end subroutine
!---------------------------------
subroutine init_itv_53
       !+ad_varc  <LI> (53) fjprot (f-value for equation 35)
       lablxc(53) = 'fjprot        '
      boundl(53) = 0.001D0
      boundu(53) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_53()
   itv_53 = fjprot 
end function

subroutine set_itv_53(ratio)
  real(kind(1.d0))::ratio
  fjprot = ratio
end subroutine
!---------------------------------
subroutine init_itv_54
       !+ad_varc  <LI> (54) ftmargtf (f-value for equation 36)
       lablxc(54) = 'ftmargtf      '
      boundl(54) = 0.001D0
      boundu(54) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_54()
   itv_54 = ftmargtf 
end function

subroutine set_itv_54(ratio)
  real(kind(1.d0))::ratio
  ftmargtf = ratio
end subroutine
!---------------------------------
! 55 not used
!---------------------------------
subroutine init_itv_56
       !+ad_varc  <LI> (56) tdmptf
       lablxc(56) = 'tdmptf        '
      boundl(56) = 0.100D0
      boundu(56) = 100.0D0
end subroutine

real(kind(1.d0)) function itv_56()
   itv_56 = tdmptf 
end function

subroutine set_itv_56(ratio)
  real(kind(1.d0))::ratio
  tdmptf = ratio
end subroutine
!---------------------------------
subroutine init_itv_57
       !+ad_varc  <LI> (57) thkcas
       lablxc(57) = 'thkcas        '
      boundl(57) = 0.050D0
      boundu(57) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_57()
   itv_57 = thkcas 
end function

subroutine set_itv_57(ratio)
  real(kind(1.d0))::ratio
  thkcas = ratio
end subroutine
!---------------------------------
subroutine init_itv_58
       !+ad_varc  <LI> (58) thwcndut
       lablxc(58) = 'thwcndut      '
      boundl(58) = 0.001D0
      boundu(58) = 0.100D0
end subroutine

real(kind(1.d0)) function itv_58()
   itv_58 = thwcndut 
end function

subroutine set_itv_58(ratio)
  real(kind(1.d0))::ratio
  thwcndut = ratio
end subroutine
!---------------------------------
subroutine init_itv_59
       !+ad_varc  <LI> (59) fcutfsu
       lablxc(59) = 'fcutfsu       '
      boundl(59) = 0.001D0
      boundu(59) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_59()
   itv_59 = fcutfsu 
end function

subroutine set_itv_59(ratio)
  real(kind(1.d0))::ratio
  fcutfsu = ratio
end subroutine
!---------------------------------
subroutine init_itv_60
       !+ad_varc  <LI> (60) cpttf
       lablxc(60) = 'cpttf         '
      boundl(60) = 0.001D0
      boundu(60) = 4.000D4
end subroutine

real(kind(1.d0)) function itv_60()
   itv_60 = cpttf 
end function

subroutine set_itv_60(ratio)
  real(kind(1.d0))::ratio
  cpttf = ratio
end subroutine
!---------------------------------
subroutine init_itv_61
       !+ad_varc  <LI> (61) gapds
       lablxc(61) = 'gapds         '
      boundl(61) = 0.001D0
      boundu(61) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_61()
   itv_61 = gapds 
end function

subroutine set_itv_61(ratio)
  real(kind(1.d0))::ratio
  gapds = ratio
end subroutine
!---------------------------------
subroutine init_itv_62
       !+ad_varc  <LI> (62) fdtmp (f-value for equation 38)
       lablxc(62) = 'fdtmp         '
      boundl(62) = 0.001D0
      boundu(62) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_62()
   itv_62 = fdtmp 
end function

subroutine set_itv_62(ratio)
  real(kind(1.d0))::ratio
  fdtmp = ratio
end subroutine
!---------------------------------
subroutine init_itv_63
       !+ad_varc  <LI> (63) ftpeak (f-value for equation 39)
       lablxc(63) = 'ftpeak        '
      boundl(63) = 0.001D0
      boundu(63) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_63()
   itv_63 = ftpeak 
end function

subroutine set_itv_63(ratio)
  real(kind(1.d0))::ratio
  ftpeak = ratio
end subroutine
!---------------------------------
subroutine init_itv_64
       !+ad_varc  <LI> (64) fauxmn (f-value for equation 40)
       lablxc(64) = 'fauxmn        '
      boundl(64) = 0.001D0
      boundu(64) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_64()
   itv_64 = fauxmn 
end function

subroutine set_itv_64(ratio)
  real(kind(1.d0))::ratio
  fauxmn = ratio
end subroutine
!---------------------------------
subroutine init_itv_65
       !+ad_varc  <LI> (65) tohs
       lablxc(65) = 'tohs          '
      boundl(65) = 0.100D0
      boundu(65) = 1.000D3
end subroutine

real(kind(1.d0)) function itv_65()
   itv_65 = tohs 
end function

subroutine set_itv_65(ratio)
  real(kind(1.d0))::ratio
  tohs = ratio
end subroutine
!---------------------------------
subroutine init_itv_66
       !+ad_varc  <LI> (66) ftohs (f-value for equation 41)
       lablxc(66) = 'ftohs         '
      boundl(66) = 0.001D0
      boundu(66) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_66()
   itv_66 = ftohs 
end function

subroutine set_itv_66(ratio)
  real(kind(1.d0))::ratio
  ftohs = ratio
end subroutine
!---------------------------------
subroutine init_itv_67
       !+ad_varc  <LI> (67) ftcycl (f-value for equation 42)
       lablxc(67) = 'ftcycl        '
      boundl(67) = 0.001D0
      boundu(67) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_67()
   itv_67 = ftcycl 
end function

subroutine set_itv_67(ratio)
  real(kind(1.d0))::ratio
  ftcycl = ratio
end subroutine
!---------------------------------
subroutine init_itv_68
       !+ad_varc  <LI> (68) fptemp (f-value for equation 44)
       lablxc(68) = 'fptemp        '
      boundl(68) = 0.001D0
      boundu(68) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_68()
   itv_68 = fptemp 
end function

subroutine set_itv_68(ratio)
  real(kind(1.d0))::ratio
  fptemp= ratio
end subroutine
!---------------------------------
subroutine init_itv_69
       !+ad_varc  <LI> (69) rcool
       lablxc(69) = 'rcool         '
      boundl(69) = 0.001D0
      boundu(69) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_69()
   itv_69 = rcool 
end function

subroutine set_itv_69(ratio)
  real(kind(1.d0))::ratio
  rcool = ratio
end subroutine
!---------------------------------
subroutine init_itv_70
       !+ad_varc  <LI> (70) vcool
       lablxc(70) = 'vcool         '
      boundl(70) = 1.000D0
      boundu(70) = 1.000D2
end subroutine

real(kind(1.d0)) function itv_70()
   itv_70 = vcool 
end function

subroutine set_itv_70(ratio)
  real(kind(1.d0))::ratio
  vcool = ratio
end subroutine
!---------------------------------
subroutine init_itv_71
       !+ad_varc  <LI> (71) fq (f-value for equation 45)
       lablxc(71) = 'fq            '
      boundl(71) = 0.001D0
      boundu(71) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_71()
   itv_71 = fq 
end function

subroutine set_itv_71(ratio)
  real(kind(1.d0))::ratio
  fq = ratio
end subroutine
!---------------------------------
subroutine init_itv_72
       !+ad_varc  <LI> (72) fipir (f-value for equation 46)
       lablxc(72) = 'fipir         '
      boundl(72) = 0.001D0
      boundu(72) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_72()
   itv_72 = fipir 
end function

subroutine set_itv_72(ratio)
  real(kind(1.d0))::ratio
  fipir = ratio
end subroutine
!---------------------------------
subroutine init_itv_73
       !+ad_varc  <LI> (73) scrapli
       lablxc(73) = 'scrapli       '
      boundl(73) = 0.001D0
      boundu(73) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_73()
   itv_73 = scrapli 
end function

subroutine set_itv_73(ratio)
  real(kind(1.d0))::ratio
  scrapli = ratio
end subroutine
!---------------------------------
subroutine init_itv_74
       !+ad_varc  <LI> (74) scraplo
       lablxc(74) = 'scraplo       '
      boundl(74) = 0.001D0
      boundu(74) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_74()
   itv_74 = scraplo 
end function

subroutine set_itv_74(ratio)
  real(kind(1.d0))::ratio
  scraplo = ratio
end subroutine
!---------------------------------
subroutine init_itv_75
       !+ad_varc  <LI> (75) tfootfi
       lablxc(75) = 'tfootfi       '
      boundl(75) = 0.200D0
      boundu(75) = 5.000D0
end subroutine

real(kind(1.d0)) function itv_75()
   itv_75 = tfootfi 
end function

subroutine set_itv_75(ratio)
  real(kind(1.d0))::ratio
  tfootfi = ratio
end subroutine
!---------------------------------
! 76 not used
! 77 not used
! 78 not used 
!---------------------------------
subroutine init_itv_79
       !+ad_varc  <LI> (79) fbetap (f-value for equation 48)
       lablxc(79) = 'fbetap        '
      boundl(79) = 0.001D0
      boundu(79) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_79()
   itv_79 = fbetap 
end function

subroutine set_itv_79(ratio)
  real(kind(1.d0))::ratio
  fbetap = ratio
end subroutine
!---------------------------------
! 80 - 88 not used
!---------------------------------
subroutine init_itv_89
       !+ad_varc  <LI> (89) ftbr (f-value for equation 52)
       lablxc(89) = 'ftbr          '
      boundl(89) = 0.001D0
      boundu(89) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_89()
   itv_89 = ftbr 
end function

subroutine set_itv_89(ratio)
  real(kind(1.d0))::ratio
  ftbr = ratio
end subroutine
!---------------------------------
subroutine init_itv_90
       !+ad_varc  <LI> (90) blbuith
       lablxc(90) = 'blbuith       '
      boundl(90) = 0.001D0
      boundu(90) = 2.000D0
end subroutine

real(kind(1.d0)) function itv_90()
   itv_90 = blbuith 
end function

subroutine set_itv_90(ratio)
  real(kind(1.d0))::ratio
  blbuith = ratio
end subroutine
!---------------------------------
subroutine init_itv_91
       !+ad_varc  <LI> (91) blbuoth
       lablxc(91) = 'blbuoth       '
      boundl(91) = 0.001D0
      boundu(91) = 2.000D0
end subroutine

real(kind(1.d0)) function itv_91()
   itv_91 = blbuoth 
end function

subroutine set_itv_91(ratio)
  real(kind(1.d0))::ratio
  blbuoth = ratio
end subroutine
!---------------------------------
subroutine init_itv_92
       !+ad_varc  <LI> (92) fflutf (f-value for equation 53)
       lablxc(92) = 'fflutf        '
      boundl(92) = 0.001D0
      boundu(92) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_92()
   itv_92 = fflutf 
end function

subroutine set_itv_92(ratio)
  real(kind(1.d0))::ratio
  fflutf = ratio
end subroutine
!---------------------------------
subroutine init_itv_93
       !+ad_varc  <LI> (93) shldith
       lablxc(93) = 'shldith       '
      boundl(93) = 0.001D0
      boundu(93) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_93()
   itv_93 = shldith 
end function

subroutine set_itv_93(ratio)
  real(kind(1.d0))::ratio
  shldith = ratio
end subroutine
!---------------------------------
subroutine init_itv_94
       !+ad_varc  <LI> (94) shldoth
       lablxc(94) = 'shldoth       '
      boundl(94) = 0.001D0
      boundu(94) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_94()
   itv_94 = shldoth 
end function

subroutine set_itv_94(ratio)
  real(kind(1.d0))::ratio
  shldoth = ratio
end subroutine
!---------------------------------
subroutine init_itv_95
       !+ad_varc  <LI> (95) fptfnuc (f-value for equation 54)
       lablxc(95) = 'fptfnuc       '
      boundl(95) = 0.001D0
      boundu(95) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_95()
   itv_95 = fptfnuc 
end function

subroutine set_itv_95(ratio)
  real(kind(1.d0))::ratio
  fptfnuc = ratio
end subroutine
!---------------------------------
subroutine init_itv_96
       !+ad_varc  <LI> (96) fvvhe (f-value for equation 55)
       lablxc(96) = 'fvvhe         '
      boundl(96) = 0.001D0
      boundu(96) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_96()
   itv_96 = fvvhe 
end function

subroutine set_itv_96(ratio)
  real(kind(1.d0))::ratio
  fvvhe = ratio
end subroutine
!---------------------------------
subroutine init_itv_97
       !+ad_varc  <LI> (97) fpsepr (f-value for equation 56)
       lablxc(97) = 'fpsepr        '
      boundl(97) = 0.001D0
      boundu(97) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_97()
   itv_97 = fpsepr 
end function

subroutine set_itv_97(ratio)
  real(kind(1.d0))::ratio
  fpsepr = ratio
end subroutine
!---------------------------------
subroutine init_itv_98
       !+ad_varc  <LI> (98) li6enrich
       lablxc(98) = 'li6enrich     '
      boundl(98) = 10.00D0
      boundu(98) = 100.0D0
end subroutine

real(kind(1.d0)) function itv_98()
   itv_98 = li6enrich 
end function

subroutine set_itv_98(ratio)
  real(kind(1.d0))::ratio
  li6enrich = ratio
end subroutine
!---------------------------------
! 99 - 101 not used
!---------------------------------
subroutine init_itv_102
       !+ad_varc  <LI> (102) fimpvar
       lablxc(102) = 'fimpvar       '
      boundl(102) = 1.00D-6
      boundu(102) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_102()
   itv_102 =  fimpvar
end function

subroutine set_itv_102(ratio)
  real(kind(1.d0))::ratio
  fimpvar = ratio
  impurity_arr(impvar)%frac = fimpvar
end subroutine
!---------------------------------
subroutine init_itv_103
       !+ad_varc  <LI> (103) flhthresh (f-value for equation 15)
       lablxc(103) = 'flhthresh     '
      boundl(103) = 1.000D0
      boundu(103) = 1.000D6
end subroutine

real(kind(1.d0)) function itv_103()
   itv_103 = flhthresh 
end function

subroutine set_itv_103(ratio)
  real(kind(1.d0))::ratio
  flhthresh = ratio
end subroutine
!---------------------------------
subroutine init_itv_104
       !+ad_varc  <LI> (104) fcwr (f-value for equation 23)
       lablxc(104) = 'fcwr          '
      boundl(104) = 0.001D0
      boundu(104) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_104()
   itv_104 = fcwr 
end function

subroutine set_itv_104(ratio)
  real(kind(1.d0))::ratio
  fcwr = ratio
end subroutine
!---------------------------------
subroutine init_itv_105
       !+ad_varc  <LI> (105) fnbshinef (f-value for equation 59)
       lablxc(105) = 'fnbshinef     '
      boundl(105) = 0.001D0
      boundu(105) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_105()
   itv_105 = fnbshinef 
end function

subroutine set_itv_105(ratio)
  real(kind(1.d0))::ratio
  fnbshinef = ratio
end subroutine
!---------------------------------
subroutine init_itv_106
       !+ad_varc  <LI> (106) ftmargoh (f-value for equation 60)
       lablxc(106) = 'ftmargoh      '
      boundl(106) = 0.001D0
      boundu(106) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_106()
   itv_106 = ftmargoh 
end function

subroutine set_itv_106(ratio)
  real(kind(1.d0))::ratio
  ftmargoh = ratio
end subroutine
!---------------------------------
subroutine init_itv_107
       !+ad_varc  <LI> (107) favail (f-value for equation 61)
       lablxc(107) = 'favail        '
      boundl(107) = 0.001D0
      boundu(107) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_107()
   itv_107 = favail 
end function

subroutine set_itv_107(ratio)
  real(kind(1.d0))::ratio
  favail = ratio
end subroutine
!---------------------------------
subroutine init_itv_108
       !+ad_varc  <LI> (108) breeder_f: Volume of Li4SiO4 / (Volume of Be12Ti + Li4SiO4)
       lablxc(108) = 'breeder_f     '
      boundl(108) = 0.060D0
      boundu(108) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_108()
   itv_108 = breeder_f 
end function

subroutine set_itv_108(ratio)
  real(kind(1.d0))::ratio
  breeder_f = ratio
end subroutine
!---------------------------------
subroutine init_itv_109
       !+ad_varc  <LI> (109) ralpne: thermal alpha density / electron density
       lablxc(109) = 'ralpne        '
      boundl(109) = 0.050D0
      boundu(109) = 0.150D0
end subroutine

real(kind(1.d0)) function itv_109()
   itv_109 = ralpne 
end function

subroutine set_itv_109(ratio)
  real(kind(1.d0))::ratio
  ralpne = ratio
end subroutine
!---------------------------------
subroutine init_itv_110
       !+ad_varc  <LI> (110) ftaulimit: Lower limit on taup/taueff the ratio of alpha particle
       !+ad_varc       to energy confinement times (f-value for equation 62)
       lablxc(110) = 'ftaulimit     '
      boundl(110) = 0.001D0
      boundu(110) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_110()
   itv_110 = ftaulimit 
end function

subroutine set_itv_110(ratio)
  real(kind(1.d0))::ratio
  ftaulimit = ratio
end subroutine
!---------------------------------
subroutine init_itv_111
       !+ad_varc  <LI> (111) fniterpump: f-value for constraint that
       !+ad_varc       number of vacuum pumps <  TF coils (f-value for equation 63)
       lablxc(111) = 'fniterpump    '
      boundl(111) = 0.001D0
      boundu(111) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_111()
   itv_111 = fniterpump 
end function

subroutine set_itv_111(ratio)
  real(kind(1.d0))::ratio
  fniterpump = ratio
end subroutine
!---------------------------------
subroutine init_itv_112
       !+ad_varc  <LI> (112) fzeffmax: f-value for max Zeff (f-value for equation 64)
       lablxc(112) = 'fzeffmax      '
      boundl(112) = 0.001D0
      boundu(112) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_112()
   itv_112 =  fzeffmax
end function

subroutine set_itv_112(ratio)
  real(kind(1.d0))::ratio
  fzeffmax = ratio
end subroutine
!---------------------------------
subroutine init_itv_113
       !+ad_varc  <LI> (113) ftaucq: f-value for minimum quench time (f-value for equation 65)
       lablxc(113) = 'ftaucq        '
      boundl(113) = 0.001D0
      boundu(113) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_113()
   itv_113 = ftaucq 
end function

subroutine set_itv_113(ratio)
  real(kind(1.d0))::ratio
  ftaucq = ratio
end subroutine
!---------------------------------
subroutine init_itv_114
       !+ad_varc  <LI> (114) fw_channel_length: Length of a single first wall channel
       lablxc(114) = 'fw_channel_length  '
      boundl(114) = 0.001D0
      boundu(114) = 1.000D3
end subroutine

real(kind(1.d0)) function itv_114()
   itv_114 = fw_channel_length
end function

subroutine set_itv_114(ratio)
  real(kind(1.d0))::ratio
  fw_channel_length = ratio
end subroutine
!---------------------------------
subroutine init_itv_115
       !+ad_varc  <LI> (115) fpoloidalpower: f-value for max rate of change of energy in poloidal field
       !+ad_varc             (f-value for equation 66)
       lablxc(115) = 'fpoloidalpower'
      boundl(26) = 0.001D0
      boundu(26) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_115()
   itv_115 = fpoloidalpower 
end function

subroutine set_itv_115(ratio)
  real(kind(1.d0))::ratio
  fpoloidalpower = ratio
end subroutine
!---------------------------------
subroutine init_itv_116
       !+ad_varc  <LI> (116) fradwall: f-value for radiation wall load limit (eq. 67)
       lablxc(116) = 'fradwall      '
      boundl(116) = 0.001D0
      boundu(116) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_116()
   itv_116 = fradwall 
end function

subroutine set_itv_116(ratio)
  real(kind(1.d0))::ratio
  fradwall = ratio
end subroutine
!---------------------------------
subroutine init_itv_117
       !+ad_varc  <LI> (117) fpsepbqar: f-value for  Psep*Bt/qar upper limit (eq. 68)
       lablxc(117) = 'fpsepbqar     '
      boundl(117) = 0.001D0
      boundu(117) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_117()
   itv_117 = fpsepbqar 
end function

subroutine set_itv_117(ratio)
  real(kind(1.d0))::ratio
  fpsepbqar = ratio
end subroutine
!---------------------------------
subroutine init_itv_118
       !+ad_varc  <LI> (118) fpsep: f-value to ensure separatrix power is less than value from Kallenbach divertor
       !+ad_varc            (f-value for equation 69)
       lablxc(118) = 'fpsep         '
      boundl(118) = 0.001D0
      boundu(118) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_118()
   itv_118 = fpsep 
end function

subroutine set_itv_118(ratio)
  real(kind(1.d0))::ratio
  fpsep = ratio
end subroutine
!---------------------------------
subroutine init_itv_119
       !+ad_varc  <LI> (119) tesep:  separatrix temperature calculated by the Kallenbach divertor model
       lablxc(119) = 'tesep         '
      boundl(119) = 0.000D0
      boundu(119) = 1.000D1
end subroutine

real(kind(1.d0)) function itv_119()
   itv_119 = tesep 
end function

subroutine set_itv_119(ratio)
  real(kind(1.d0))::ratio
  tesep = ratio
end subroutine
!---------------------------------
subroutine init_itv_120
       !+ad_varc  <LI> (120) ttarget: Plasma temperature adjacent to divertor sheath [eV]
       lablxc(120) = 'ttarget       '
      boundl(120) = 1.000D0
      boundu(120) = 1.000D4
end subroutine

real(kind(1.d0)) function itv_120()
   itv_120 =  ttarget
end function

subroutine set_itv_120(ratio)
  real(kind(1.d0))::ratio
  ttarget = ratio
end subroutine
!---------------------------------
subroutine init_itv_121
       !+ad_varc  <LI> (121) neratio: ratio of mean SOL density at OMP to separatrix density at OMP
       lablxc(121) = 'neratio       '
      boundl(121) = 0.001D0
      boundu(121) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_121()
   itv_121 =  neratio
end function

subroutine set_itv_121(ratio)
  real(kind(1.d0))::ratio
  neratio = ratio
end subroutine
!---------------------------------
subroutine init_itv_122
       !+ad_varc  <LI> (122) oh_steel_frac : streel fraction of Central Solenoid
       lablxc(122) = 'oh_steel_frac '
      boundl(122) = 0.001D0
      boundu(122) = 0.950D0
end subroutine

real(kind(1.d0)) function itv_122()
   itv_122 = oh_steel_frac 
end function

subroutine set_itv_122(ratio)
  real(kind(1.d0))::ratio
  oh_steel_frac = ratio
end subroutine
!---------------------------------
subroutine init_itv_123
       !+ad_varc  <LI> (123) foh_stress : f-value for CS coil Tresca stress limit (f-value for eq. 72)
       lablxc(123) = 'foh_stress    '
      boundl(123) = 0.001D0
      boundu(123) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_123()
   itv_123 = foh_stress 
end function

subroutine set_itv_123(ratio)
  real(kind(1.d0))::ratio
  foh_stress = ratio
end subroutine
!---------------------------------
subroutine init_itv_124
       !+ad_varc  <LI> (124) qtargettotal : Power density on target including surface recombination [W/m2]
       lablxc(124) = 'qtargettotal  '
      boundl(124) = 0.001D0
      boundu(124) = 1.000D7
end subroutine

real(kind(1.d0)) function itv_124()
   itv_124 = qtargettotal 
end function

subroutine set_itv_124(ratio)
  real(kind(1.d0))::ratio
  qtargettotal = ratio
end subroutine
!---------------------------------
subroutine init_itv_125
       !+ad_varc  <LI> (125) fimp(3) :  Beryllium density fraction relative to electron density
       lablxc(125) = 'fimp(03)      '
      boundl(125) = 1.00D-8
      boundu(125) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_125()
   itv_125 = fimp(03) 
end function

subroutine set_itv_125(ratio)
  real(kind(1.d0))::ratio
  fimp(03) = ratio
end subroutine
!---------------------------------
subroutine init_itv_126
       !+ad_varc  <LI> (126) fimp(4) :  Carbon density fraction relative to electron density
       lablxc(126) = 'fimp(04)      '
      boundl(126) = 1.00D-8
      boundu(126) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_126()
   itv_126 = fimp(04) 
end function

subroutine set_itv_126(ratio)
  real(kind(1.d0))::ratio
  fimp(04) = ratio
end subroutine
!---------------------------------
subroutine init_itv_127
       !+ad_varc  <LI> (127) fimp(5) :  Nitrogen fraction relative to electron density
       lablxc(127) = 'fimp(05)      '
      boundl(127) = 1.00D-8
      boundu(127) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_127()
   itv_127 = fimp(05) 
end function

subroutine set_itv_127(ratio)
  real(kind(1.d0))::ratio
  fimp(05) = ratio
end subroutine
!---------------------------------
subroutine init_itv_128
       !+ad_varc  <LI> (128) fimp(6) :  Oxygen density fraction relative to electron density
       lablxc(128) = 'fimp(06)      '
      boundl(128) = 1.00D-8
      boundu(128) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_128()
   itv_128 = fimp(06) 
end function

subroutine set_itv_128(ratio)
  real(kind(1.d0))::ratio
  fimp(06) = ratio
end subroutine
!---------------------------------
subroutine init_itv_129
       !+ad_varc  <LI> (129) fimp(7) :  Neon density fraction relative to electron density
       lablxc(129) = 'fimp(07)      '
      boundl(129) = 1.00D-8
      boundu(129) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_129()
   itv_129 = fimp(07) 
end function

subroutine set_itv_129(ratio)
  real(kind(1.d0))::ratio
  fimp(07) = ratio
end subroutine
!---------------------------------
subroutine init_itv_130
       !+ad_varc  <LI> (130) fimp(8) :  Silicon density fraction relative to electron density
       lablxc(130) = 'fimp(08)      '
      boundl(130) = 1.00D-8
      boundu(130) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_130()
   itv_130 = fimp(08) 
end function

subroutine set_itv_130(ratio)
  real(kind(1.d0))::ratio
  fimp(08) = ratio
end subroutine
!---------------------------------
subroutine init_itv_131
       !+ad_varc  <LI> (131) fimp(9) :  Argon density fraction relative to electron density
       lablxc(131) = 'fimp(09)      '
      boundl(131) = 1.00D-8
      boundu(131) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_131()
   itv_131 = fimp(09) 
end function

subroutine set_itv_131(ratio)
  real(kind(1.d0))::ratio
  fimp(09) = ratio
end subroutine
!---------------------------------
subroutine init_itv_132
       !+ad_varc  <LI> (132) fimp(10) :  Iron density fraction relative to electron density
       lablxc(132) = 'fimp(10)      '
      boundl(132) = 1.00D-8
      boundu(132) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_132()
   itv_132 = fimp(10) 
end function

subroutine set_itv_132(ratio)
  real(kind(1.d0))::ratio
  fimp(10) = ratio
end subroutine
!---------------------------------
subroutine init_itv_133
       !+ad_varc  <LI> (133) fimp(11) :  Nickel density fraction relative to electron density
       lablxc(133) = 'fimp(11)      '
      boundl(133) = 1.00D-8
      boundu(133) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_133()
   itv_133 = fimp(11) 
end function

subroutine set_itv_133(ratio)
  real(kind(1.d0))::ratio
  fimp(11) = ratio
end subroutine
!---------------------------------
subroutine init_itv_134
       !+ad_varc  <LI> (134) fimp(12) :  Krypton density fraction relative to electron density
       lablxc(134) = 'fimp(12)      '
      boundl(134) = 1.00D-8
      boundu(134) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_134()
   itv_134 = fimp(12) 
end function

subroutine set_itv_134(ratio)
  real(kind(1.d0))::ratio
  fimp(12) = ratio
end subroutine
!---------------------------------
subroutine init_itv_135
       !+ad_varc  <LI> (135) fimp(13) :  Xenon density fraction relative to electron density
       lablxc(135) = 'fimp(13)      '
      boundl(135) = 1.00D-8
      boundu(135) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_135()
   itv_135 = fimp(13) 
end function

subroutine set_itv_135(ratio)
  real(kind(1.d0))::ratio
  fimp(13) = ratio
end subroutine
!---------------------------------
subroutine init_itv_136
       !+ad_varc  <LI> (136) fimp(14) :  Tungsten density fraction relative to electron density
       lablxc(136) = 'fimp(14)      '
      boundl(136) = 1.00D-8
      boundu(136) = 0.010D0
end subroutine

real(kind(1.d0)) function itv_136()
   itv_136 = fimp(14) 
end function

subroutine set_itv_136(ratio)
  real(kind(1.d0))::ratio
  fimp(14) = ratio
end subroutine
!---------------------------------
subroutine init_itv_137
       !+ad_varc  <LI> (137) fplhsep (f-value for equation 73)
       lablxc(137) = 'fplhsep       '
      boundl(137) = 0.001D0
      boundu(137) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_137()
   itv_137 = fplhsep 
end function

subroutine set_itv_137(ratio)
  real(kind(1.d0))::ratio
  fplhsep = ratio
end subroutine
!---------------------------------
subroutine init_itv_138
       !+ad_varc  <LI> (138) rebco_thickness : thickness of REBCO layer in tape (m)
       lablxc(138) = 'rebco_thicknes'
      boundl(138) = 0.01D-6
      boundu(138) = 100.0D-6
end subroutine

real(kind(1.d0)) function itv_138()
   itv_138 = fplhsep 
end function

subroutine set_itv_138(ratio)
  real(kind(1.d0))::ratio
  fplhsep = ratio
end subroutine
!---------------------------------
subroutine init_itv_139
       !+ad_varc  <LI> (139) copper_thick : thickness of copper layer in tape (m)
       lablxc(139) = 'copper_thick  '
      boundl(139) = 1.00D-6
      boundu(139) = 1.00D-3
end subroutine

real(kind(1.d0)) function itv_139()
   itv_139 = copper_thick 
end function

subroutine set_itv_139(ratio)
  real(kind(1.d0))::ratio
  copper_thick = ratio
end subroutine
!---------------------------------
subroutine init_itv_140
       !+ad_varc  <LI> (140) thkwp : radial thickness of TFC winding pack (m)
       lablxc(140) = 'thkwp         '
      boundl(140) = 0.001D0
      boundu(140) = 2.000D0
end subroutine

real(kind(1.d0)) function itv_140()
   itv_140 = thkwp 
end function

subroutine set_itv_140(ratio)
  real(kind(1.d0))::ratio
  thkwp = ratio
end subroutine
!---------------------------------
subroutine init_itv_141
       !+ad_varc  <LI> (141) fcqt : TF coil quench temperature < tmax_croco (f-value for equation 74)
       lablxc(141) = 'fcqt          '
      boundl(141) = 0.001D0
      boundu(141) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_141()
   itv_141 = fcqt 
end function

subroutine set_itv_141(ratio)
  real(kind(1.d0))::ratio
  fcqt = ratio
end subroutine
!---------------------------------
subroutine init_itv_142
       !+ad_varc  <LI> (142) nesep : electron density at separatrix [m-3]
       lablxc(142) = 'nesep         '
      boundl(142) = 1.00D17
      boundu(142) = 1.00D20
end subroutine

real(kind(1.d0)) function itv_142()
   itv_142 = nesep 
end function

subroutine set_itv_142(ratio)
  real(kind(1.d0))::ratio
  nesep = ratio
end subroutine
!---------------------------------
subroutine init_itv_143
       !+ad_varc  <LI> (143) f_copperA_m2 : TF coil current / copper area < Maximum value (f-value for equation 75)
       lablxc(143) = 'f_copperA_m2  '
      boundl(143) = 0.001D0
      boundu(143) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_143()
   itv_143 = f_copperA_m2 
end function

subroutine set_itv_143(ratio)
  real(kind(1.d0))::ratio
  f_copperA_m2 = ratio
end subroutine
!---------------------------------
subroutine init_itv_144
       !+ad_varc  <LI> (144) fnesep : Eich critical electron density at separatrix (f-value for constraint equation 76) [m-3]
       lablxc(144) = 'fnesep        '
      boundl(144) = 0.001D0
      boundu(144) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_144()
   itv_144 = fnesep 
end function

subroutine set_itv_144(ratio)
  real(kind(1.d0))::ratio
  fnesep = ratio
end subroutine
!---------------------------------
subroutine init_itv_145
       !+ad_varc  <LI> (145) fgwped :  fraction of Greenwald density to set as pedestal-top density
       lablxc(145) = 'fgwped        '
      boundl(145) = 0.500D0
      boundu(145) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_145()
   itv_145 = fgwped 
end function

subroutine set_itv_145(ratio)
  real(kind(1.d0))::ratio
  fgwped = ratio
end subroutine
!---------------------------------
subroutine init_itv_146
       !+ad_varc  <LI> (146) fcpttf : F-value for TF coil current per turn limit (constraint equation 77)
       lablxc(146) = 'fcpttf        '
      boundl(146) = 0.001D0
      boundu(146) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_146()
   itv_146 = fcpttf 
end function

subroutine set_itv_146(ratio)
  real(kind(1.d0))::ratio
  fcpttf = ratio
end subroutine
!---------------------------------
subroutine init_itv_147
       !+ad_varc  <LI> (147) freinke : F-value for Reinke detachment criterion (constraint equation 78)
       lablxc(147) = 'freinke       '
      boundl(147) = 0.001D0
      boundu(147) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_147()
   itv_147 = freinke 
end function

subroutine set_itv_147(ratio)
  real(kind(1.d0))::ratio
  freinke = ratio
end subroutine
!---------------------------------
subroutine init_itv_148
       !+ad_varc  <LI> (148) fzactual : fraction of impurity at SOL with Reinke detachment criterion
       lablxc(148) = 'fzactual      '
      boundl(148) = 1.00D-8
      boundu(148) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_148()
   ! TODO This needs to be clarified!! MDK
   ! It looks like fzactual is not really an iteration variable.
   itv_148 = impurity_arr(impvardiv)%frac*impurity_enrichment(impvardiv)   !fzactual
end function

subroutine set_itv_148(ratio)
  real(kind(1.d0))::ratio
  fzactual = ratio
  impurity_arr(impvardiv)%frac = fzactual / impurity_enrichment(impvardiv)
end subroutine
!---------------------------------
subroutine init_itv_149
       !+ad_varc  <LI> (149) fbmaxcs : F-value for max peak CS field (con. 79, itvar 149)
       lablxc(149) = 'fbmaxcs       '
      boundl(149) = 0.001D0
      boundu(149) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_149()
   itv_149 = fbmaxcs 
end function

subroutine set_itv_149(ratio)
  real(kind(1.d0))::ratio
  fbmaxcs = ratio
end subroutine
!---------------------------------
subroutine init_itv_150
        !+ad_varc  <LI> (150) plasmod_fcdp : (P_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD power over available power
       lablxc(150) = 'plasmod_fcdp  '
      boundl(150) = 0.000D0
      boundu(150) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_150()
   itv_150 = plasmod_fcdp 
end function

subroutine set_itv_150(ratio)
  real(kind(1.d0))::ratio
  plasmod_fcdp = ratio
end subroutine
!---------------------------------
subroutine init_itv_151
       !+ad_varc  <LI> (151) plasmod_fradc : Pline_Xe / (Palpha + Paux - PlineAr - Psync - Pbrad)
       lablxc(151) = 'plasmod_fradc '
      boundl(151) = 0.001D0
      boundu(151) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_151()
   itv_151 = plasmod_fradc 
end function

subroutine set_itv_151(ratio)
  real(kind(1.d0))::ratio
  plasmod_fradc = ratio
end subroutine
!---------------------------------
subroutine init_itv_152
       !+ad_varc  <LI> (152) fbmaxcs : Ratio of separatrix density to Greenwald density
       lablxc(152) = 'fgwsep        '
      boundl(152) = 0.001D0
      boundu(152) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_152()
   itv_152 = fgwsep 
end function

subroutine set_itv_152(ratio)
  real(kind(1.d0))::ratio
  fgwsep = ratio
end subroutine
!---------------------------------
subroutine init_itv_153
       !+ad_varc  <LI> (153) fpdivlim : F-value for minimum pdivt (con. 80)
       lablxc(153) = 'fpdivlim      '
      boundl(153) = 0.001D0
      boundu(153) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_153()
   itv_153 = fpdivlim
end function

subroutine set_itv_153(ratio)
  real(kind(1.d0))::ratio
  fpdivlim = ratio
end subroutine
!---------------------------------
subroutine init_itv_154
       !+ad_varc  <LI> (154) fne0 : F-value for ne(0) > ne(ped) (con. 81)
       lablxc(154) = 'fpdivlim      '
      boundl(154) = 0.001D0
      boundu(154) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_154()
   itv_154 = fpdivlim 
end function

subroutine set_itv_154(ratio)
  real(kind(1.d0))::ratio
  fpdivlim = ratio
end subroutine
!---------------------------------
subroutine init_itv_155
end subroutine

real(kind(1.d0)) function itv_155()
   itv_155 = DUMMY 
end function

subroutine set_itv_155(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_156
end subroutine

real(kind(1.d0)) function itv_156()
   itv_156 = DUMMY 
end function

subroutine set_itv_156(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_157
end subroutine

real(kind(1.d0)) function itv_157()
   itv_157 = DUMMY 
end function

subroutine set_itv_157(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_158
end subroutine

real(kind(1.d0)) function itv_158()
   itv_158 = DUMMY 
end function

subroutine set_itv_158(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_159
end subroutine

real(kind(1.d0)) function itv_159()
   itv_159 = DUMMY 
end function

subroutine set_itv_159(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_160
end subroutine

real(kind(1.d0)) function itv_160()
   itv_160 = DUMMY 
end function

subroutine set_itv_160(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_161
end subroutine

real(kind(1.d0)) function itv_161()
   itv_161 = DUMMY 
end function

subroutine set_itv_161(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_162
end subroutine

real(kind(1.d0)) function itv_162()
   itv_162 = DUMMY 
end function

subroutine set_itv_162(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_163
end subroutine

real(kind(1.d0)) function itv_163()
   itv_163 = DUMMY 
end function

subroutine set_itv_163(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_164
end subroutine

real(kind(1.d0)) function itv_164()
   itv_164 = DUMMY 
end function

subroutine set_itv_164(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_165
end subroutine

real(kind(1.d0)) function itv_165()
   itv_165 = DUMMY 
end function

subroutine set_itv_165(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_166
end subroutine

real(kind(1.d0)) function itv_166()
   itv_166 = DUMMY 
end function

subroutine set_itv_166(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_167
end subroutine

real(kind(1.d0)) function itv_167()
   itv_167 = DUMMY 
end function

subroutine set_itv_167(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_168
end subroutine

real(kind(1.d0)) function itv_168()
   itv_168 = DUMMY 
end function

subroutine set_itv_168(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_169
end subroutine

real(kind(1.d0)) function itv_169()
   itv_169 = DUMMY 
end function

subroutine set_itv_169(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine
!---------------------------------
subroutine init_itv_170
end subroutine

real(kind(1.d0)) function itv_170()
   itv_170 = DUMMY 
end function

subroutine set_itv_170(ratio)
  real(kind(1.d0))::ratio
  DUMMY = ratio
end subroutine


!+ad_varc  </UL>
end module define_iteration_variables
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine loadxc

  !+ad_name  loadxc
  !+ad_summ  Routine to load the physics and engineering variables into the
  !+ad_summ  optimisation variables array
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This subroutine loads the physics and engineering variables
  !+ad_desc  into the optimisation variables array <CODE>XCM</CODE>.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constraint_variables
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables

  !+ad_call  impurity_radiation_module
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables

  !+ad_call  stellarator_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  pulse_variables
  !+ad_call  report_error
  !+ad_hist  22/10/92 PJK Removed original arguments (xc,nn)
  !+ad_hist  14/11/11 PJK Changed NaN error check
  !+ad_hist  09/10/12 PJK Initial F90 version
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  06/11/12 PJK Renamed source file iteration_variables.f90 from xc.f90
  !+ad_hist  04/06/13 PJK Added ftbr (89), blbuith (90), blbuoth (91),
  !+ad_hisc               fflutf (92), shldith (93), shldoth (94),
  !+ad_hisc               fptfnuc (95), fvvhe (96)
  !+ad_hist  19/06/13 PJK fjtfc (var.28) is obsolete - added error trap
  !+ad_hist  30/09/13 PJK Added fpsepr (97)
  !+ad_hist  17/10/13 PJK Modified logic for cdtfleg usage
  !+ad_hist  28/11/13 PJK Added li6enrich (98)
  !+ad_hist  26/02/14 PJK Added ftftort (99) and ftfthko (100)
  !+ad_hist  05/03/14 PJK Added warnings for usage of some iteration variables if
  !+ad_hisc               istell=1
  !+ad_hist  30/04/14 PJK Added prp (101)
  !+ad_hist  08/05/14 PJK Replaced itfmod with tfc_model
  !+ad_hist  19/05/14 PJK Reassigned (28) to fradpwr
  !+ad_hist  02/06/14 PJK Added fimpvar (102)
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  30/07/14 PJK Changed tftort clause
  !+ad_hist  01/10/14 PJK Added flhthresh (103)
  !+ad_hist  02/10/14 PJK Added fcwr (104)
  !+ad_hist  06/10/14 PJK Added fnbshinef (105)
  !+ad_hist  11/11/14 PJK Added ftmargoh (106)
  !+ad_hist  25/11/14 JM  Added favail (107)
  !+ad_hist  27/05/15 MDK Added breeder_f (108)
  !+ad_hist  03/08/15 MDK Create list of iteration variable names
  !+ad_hist  05/08/15 MDK Added ralpne, ftaulimit
  !+ad_hist  26/08/15 MDK Added fniterpump (111)
  !+ad_hist  18/11/15 RK  Added fzeffmax (112)
  !+ad_hist  26/11/15 RK  Added ftaucq (113)
  !+ad_hist  10/11/16 HL  Added fradwall (116)
  !+ad_hist  19/01/17 JM  Added fpsepbqar (117)
  !+ad_hist  08/02/17 JM  Added fpsep, tesep and ttarget  (118, 119, 120)
  !+ad_hist  22/02/17 JM  Added neratio (121)
  !+ad_hist  27/02/17 JM  Added oh_steel_frac (122)
  !+ad_hist  27/02/17 JM  Added foh_stress (123)
  !+ad_hist  15/03/17 MDK  Added qtargettotal (124)
  !+ad_hist  17/03/17 MDK  Added impurities fimp(3-14) (125-136)
  !+ad_hist  12/01/18 KE  Added fnesep (144)
  !+ad_hist  22/06/18 SIM cdtfleg (itv 24) no longer used
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constraint_variables
  use cost_variables
  use current_drive_variables
  use define_iteration_variables   
  use divertor_kallenbach_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables
  use impurity_radiation_module
  use numerics
  use pfcoil_variables
  use physics_variables
  use plasmod_variables
  use pulse_variables
  use reinke_variables
  use stellarator_variables
  use tfcoil_variables
  use times_variables
  use global_variables
  use rebco_variables

  implicit none

  !  Arguments

  !  Local variables

  integer :: i
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  do i = 1,nvar

     select case (ixc(i))
         case(1);  xcm(i) = itv_1()
         case(2);  xcm(i) = itv_2()
         case(3);  xcm(i) = itv_3()
         case(4);  xcm(i) = itv_4()
         case(5);  xcm(i) = itv_5()
         case(6);  xcm(i) = itv_6()
         case(7);  xcm(i) = itv_7()
         case(8);  xcm(i) = itv_8()
         case(9);  xcm(i) = itv_9()
         case(10);  xcm(i) = itv_10()
         case(11);  xcm(i) = itv_11()
         case(12);  xcm(i) = itv_12()
         case(13);  xcm(i) = itv_13()
         case(14);  xcm(i) = itv_14()
         case(15);  xcm(i) = itv_15()
         case(16);  xcm(i) = itv_16()
         case(17);  xcm(i) = itv_17()
         case(18);  xcm(i) = itv_18()
         case(19);  xcm(i) = itv_19()
         case(20);  xcm(i) = itv_20()
         case(21);  xcm(i) = itv_21()
         case(22);  
         case(23);  xcm(i) = itv_23()
         case(24);  
         case(25);  xcm(i) = itv_25()
         case(26);  xcm(i) = itv_26()
         case(27);  xcm(i) = itv_27()
         case(28);  xcm(i) = itv_28()
         case(29);  xcm(i) = itv_29()
         case(30);  xcm(i) = itv_30()
         case(31);  xcm(i) = itv_31()
         case(32);  xcm(i) = itv_32()
         case(33);  xcm(i) = itv_33()
         case(34);  xcm(i) = itv_34()
         case(35);  xcm(i) = itv_35()
         case(36);  xcm(i) = itv_36()
         case(37);  xcm(i) = itv_37()
         case(38);  xcm(i) = itv_38()
         case(39);  xcm(i) = itv_39()
         case(40);  xcm(i) = itv_40()
         case(41);  xcm(i) = itv_41()
         case(42);  xcm(i) = itv_42()
         case(43);  
         case(44);  xcm(i) = itv_44()
         case(45);  xcm(i) = itv_45()
         case(46);  xcm(i) = itv_46()
         case(47);  xcm(i) = itv_47()
         case(48);  xcm(i) = itv_48()
         case(49);  xcm(i) = itv_49()
         case(50);  xcm(i) = itv_50()
         case(51);  xcm(i) = itv_51()
         case(52);  xcm(i) = itv_52()
         case(53);  xcm(i) = itv_53()
         case(54);  xcm(i) = itv_54()
         case(55);  
         case(56);  xcm(i) = itv_56()
         case(57);  xcm(i) = itv_57(); if (istell == 1) call report_error(48)
         case(58);  xcm(i) = itv_58()
         case(59);  xcm(i) = itv_59()
         case(60);  xcm(i) = itv_60(); if ((istell == 1).or.(itfsup /= 1)) call report_error(49)
         case(61);  xcm(i) = itv_61()
         case(62);  xcm(i) = itv_62()
         case(63);  xcm(i) = itv_63()
         case(64);  xcm(i) = itv_64()
         case(65);  xcm(i) = itv_65(); if (lpulse /= 1) call report_error(50)
         case(66);  xcm(i) = itv_66()
         case(67);  xcm(i) = itv_67()
         case(68);  xcm(i) = itv_68()
         case(69);  xcm(i) = itv_69()
         case(70);  xcm(i) = itv_70()
         case(71);  xcm(i) = itv_71()
         case(72);  xcm(i) = itv_72()
         case(73);  xcm(i) = itv_73()
         case(74);  xcm(i) = itv_74()
         case(75);  xcm(i) = itv_75()
         case(76);  
         case(77);  
         case(78);  
         case(79);  xcm(i) = itv_79()
         case(80);  
         case(81);  
         case(82);  
         case(83);  
         case(84);  
         case(85);  
         case(86);  
         case(87);  
         case(88);  
         case(89);  xcm(i) = itv_89()
         case(90);  xcm(i) = itv_90()
         case(91);  xcm(i) = itv_91()
         case(92);  xcm(i) = itv_92()
         case(93);  xcm(i) = itv_93()
         case(94);  xcm(i) = itv_94()
         case(95);  xcm(i) = itv_95()
         case(96);  xcm(i) = itv_96()
         case(97);  xcm(i) = itv_97()
         case(98);  xcm(i) = itv_98()
         case(99);  
         case(100);  
         case(101);  
         case(102);  xcm(i) = itv_102()
         case(103);  xcm(i) = itv_103()
         case(104);  xcm(i) = itv_104()
         case(105);  xcm(i) = itv_105()
         case(106);  xcm(i) = itv_106()
         case(107);  xcm(i) = itv_107()
         case(108);  xcm(i) = itv_108()
         case(109);  xcm(i) = itv_109()
         case(110);  xcm(i) = itv_110()
         case(111);  xcm(i) = itv_111()
         case(112);  xcm(i) = itv_112()
         case(113);  xcm(i) = itv_113()
         case(114);  xcm(i) = itv_114()
         case(115);  xcm(i) = itv_115()
         case(116);  xcm(i) = itv_116()
         case(117);  xcm(i) = itv_117()
         case(118);  xcm(i) = itv_118()
         case(119);  xcm(i) = itv_119()
         case(120);  xcm(i) = itv_120()
         case(121);  xcm(i) = itv_121()
         case(122);  xcm(i) = itv_122()
         case(123);  xcm(i) = itv_123()
         case(124);  xcm(i) = itv_124()
         case(125);  xcm(i) = itv_125()
         case(126);  xcm(i) = itv_126()
         case(127);  xcm(i) = itv_127()
         case(128);  xcm(i) = itv_128()
         case(129);  xcm(i) = itv_129()
         case(130);  xcm(i) = itv_130()
         case(131);  xcm(i) = itv_131()
         case(132);  xcm(i) = itv_132()
         case(133);  xcm(i) = itv_133()
         case(134);  xcm(i) = itv_134()
         case(135);  xcm(i) = itv_135()
         case(136);  xcm(i) = itv_136()
         case(137);  xcm(i) = itv_137()
         case(138);  xcm(i) = itv_138()
         case(139);  xcm(i) = itv_139()
         case(140);  xcm(i) = itv_140()
         case(141);  xcm(i) = itv_141()
         case(142);  xcm(i) = itv_142()
         case(143);  xcm(i) = itv_143()
         case(144);  xcm(i) = itv_144()
         case(145);  xcm(i) = itv_145()
         case(146);  xcm(i) = itv_146()
         case(147);  xcm(i) = itv_147()
         case(148);  xcm(i) = itv_148()
         case(149);  xcm(i) = itv_149()
         case(150);  xcm(i) = itv_150()
         case(151);  xcm(i) = itv_151()
         case(152);  xcm(i) = itv_152()
         case(153);  xcm(i) = itv_153()
         case(154);  xcm(i) = itv_154()
         case (155);  xcm(i) = itv_155()
         case (156);  xcm(i) = itv_156()
         case (157);  xcm(i) = itv_157()
         case (158);  xcm(i) = itv_158()
         case (159);  xcm(i) = itv_159()
         case (160);  xcm(i) = itv_160()
         case (161);  xcm(i) = itv_161()
         case (162);  xcm(i) = itv_162()
         case (163);  xcm(i) = itv_163()
         case (164);  xcm(i) = itv_164()
         case (165);  xcm(i) = itv_165()
         case (166);  xcm(i) = itv_166()
         case (167);  xcm(i) = itv_167()
         case (168);  xcm(i) = itv_168()
         case (169);  xcm(i) = itv_169()
         case (170);  xcm(i) = itv_170()


   !   case (10) ; xcm(i) = hfact
   !   case (11) ; xcm(i) = pheat
   !   case (12) ; xcm(i) = oacdcp
   !   case (13) ; xcm(i) = tfcth
   !      if (istell == 1) call report_error(46)
   !   case (14) ; xcm(i) = fwalld
   !   case (15) ; xcm(i) = fvs
   !   case (16) ; xcm(i) = ohcth
   !   case (17) ; xcm(i) = tdwell
   !   case (18) ; xcm(i) = q
   !   case (19) ; xcm(i) = enbeam
   !   case (20) ; xcm(i) = tcpav
   !   case (21) ; xcm(i) = ftburn
   !   case (22) ; write(*,*) 'Iteration variable 22 is not supported.'
   !   case (23) ; xcm(i) = fcoolcp
   !   case (24) ; write(*,*) 'Iteration variable 24 is not supported.'
   !   case (25) ; xcm(i) = fpnetel
   !   case (26) ; xcm(i) = ffuspow
   !   case (27) ; xcm(i) = fhldiv
   !   case (28) ; xcm(i) = fradpwr
   !   case (29) ; xcm(i) = bore
   !   case (30) ; xcm(i) = fmva
   !   case (31) ; xcm(i) = gapomin
   !   case (32) ; xcm(i) = frminor
   !   case (33) ; xcm(i) = fportsz
   !   case (34) ; xcm(i) = fdivcol
   !   case (35) ; xcm(i) = fpeakb
   !   case (36) ; xcm(i) = fbetatry
   !   case (37) ; xcm(i) = coheof
   !   case (38) ; xcm(i) = fjohc
   !   case (39) ; xcm(i) = fjohc0
   !   case (40) ; xcm(i) = fgamcd
   !   case (41) ; xcm(i) = fcohbop
   !   case (42) ; xcm(i) = gapoh
   !   case (43) ; write(*,*) 'Iteration variable 43 is not supported.'
   !   case (44) ; xcm(i) = fvsbrnni
   !   case (45) ; xcm(i) = fqval
   !   case (46) ; xcm(i) = fpinj
   !   case (47) ; xcm(i) = feffcd
   !   case (48) ; xcm(i) = fstrcase
   !   case (49) ; xcm(i) = fstrcond
   !   case (50) ; xcm(i) = fiooic
   !   case (51) ; xcm(i) = fvdump
   !   case (52) ; xcm(i) = vdalw
   !   case (53) ; xcm(i) = fjprot
   !   case (54) ; xcm(i) = ftmargtf
   !   case (55) ; write(*,*) 'Iteration variable 55 is not supported.'
   !   case (56) ; xcm(i) = tdmptf
   !   case (57) ; xcm(i) = thkcas
   !    !   if ((tfc_model == 0).or.(istell == 1)) call report_error(48)
   !      if (istell == 1) call report_error(48)
   !   case (58) ; xcm(i) = thwcndut
   !   case (59) ; xcm(i) = fcutfsu
   !   case (60) ; xcm(i) = cpttf
   !      if ((istell == 1).or.(itfsup /= 1)) call report_error(49)
   !   case (61) ; xcm(i) = gapds
   !   case (62) ; xcm(i) = fdtmp
   !   case (63) ; xcm(i) = ftpeak
   !   case (64) ; xcm(i) = fauxmn
   !   case (65) ; xcm(i) = tohs
   !      if (lpulse /= 1) call report_error(50)
   !   case (66) ; xcm(i) = ftohs
   !   case (67) ; xcm(i) = ftcycl
   !   case (68) ; xcm(i) = fptemp
   !   case (69) ; xcm(i) = rcool
   !   case (70) ; xcm(i) = vcool
   !   case (71) ; xcm(i) = fq
   !   case (72) ; xcm(i) = fipir
   !   case (73) ; xcm(i) = scrapli
   !   case (74) ; xcm(i) = scraplo
   !   case (75) ; xcm(i) = tfootfi
   !   case (76) ; write(*,*) 'Iteration variable 76 is not supported.'
   !   case (77) ; write(*,*) 'Iteration variable 77 is not supported.'
   !   case (78) ; write(*,*) 'Iteration variable 78 is not supported.'
   !   case (79) ; xcm(i) = fbetap
   !   case (80) ; write(*,*) 'Iteration variable 80 is not supported.'
   !   case (81) ; write(*,*) 'Iteration variable 81 is not supported.'
   !   case (82) ; write(*,*) 'Iteration variable 82 is not supported.'
   !   case (83) ; write(*,*) 'Iteration variable 83 is not supported.'
   !   case (84) ; write(*,*) 'Iteration variable 84 is not supported.'
   !   case (85) ; write(*,*) 'Iteration variable 85 is not supported.'
   !   case (86) ; write(*,*) 'Iteration variable 86 is not supported.'
   !   case (87) ; write(*,*) 'Iteration variable 87 is not supported.'
   !   case (88) ; write(*,*) 'Iteration variable 87 is not supported.'
   !   case (89) ; xcm(i) = ftbr
   !   case (90) ; xcm(i) = blbuith
   !   case (91) ; xcm(i) = blbuoth
   !   case (92) ; xcm(i) = fflutf
   !   case (93) ; xcm(i) = shldith
   !   case (94) ; xcm(i) = shldoth
   !   case (95) ; xcm(i) = fptfnuc
   !   case (96) ; xcm(i) = fvvhe
   !   case (97) ; xcm(i) = fpsepr
   !   case (98) ; xcm(i) = li6enrich
   !   case (99) ; write(*,*) 'Iteration variable 99 is not supported.'
   !   case (100) ; write(*,*) 'Iteration variable 100 is not supported.'
   !   case (101) ; write(*,*) 'Iteration variable 101 is not supported.'
   !   case (102) ; xcm(i) = impurity_arr(impvar)%frac
   !   case (103) ; xcm(i) = flhthresh
   !   case (104) ; xcm(i) = fcwr
   !   case (105) ; xcm(i) = fnbshinef
   !   case (106) ; xcm(i) = ftmargoh
   !   case (107) ; xcm(i) = favail
   !   case (108) ; xcm(i) = breeder_f
   !   case (109) ; xcm(i) = ralpne
   !   case (110) ; xcm(i) = ftaulimit
   !   case (111) ; xcm(i) = fniterpump
   !   case (112) ; xcm(i) = fzeffmax
   !   case (113) ; xcm(i) = ftaucq
   !   case (114) ; xcm(i) = fw_channel_length
   !   case (115) ; xcm(i) = fpoloidalpower
   !   case (116) ; xcm(i) = fradwall
   !   case (117) ; xcm(i) = fpsepbqar
   !   case (118) ; xcm(i) = fpsep
   !   case (119) ; xcm(i) = tesep
   !   case (120) ; xcm(i) = ttarget
   !   case (121) ; xcm(i) = neratio
   !   case (122) ; xcm(i) = oh_steel_frac
   !   case (123) ; xcm(i) = foh_stress
   !   case (124) ; xcm(i) = qtargettotal
   !   case (125) ; xcm(i) = impurity_arr(3)%frac
   !   case (126) ; xcm(i) = impurity_arr(4)%frac
   !   case (127) ; xcm(i) = impurity_arr(5)%frac
   !   case (128) ; xcm(i) = impurity_arr(6)%frac
   !   case (129) ; xcm(i) = impurity_arr(7)%frac
   !   case (130) ; xcm(i) = impurity_arr(8)%frac
   !   case (131) ; xcm(i) = impurity_arr(9)%frac
   !   case (132) ; xcm(i) = impurity_arr(10)%frac
   !   case (133) ; xcm(i) = impurity_arr(11)%frac
   !   case (134) ; xcm(i) = impurity_arr(12)%frac
   !   case (135) ; xcm(i) = impurity_arr(13)%frac
   !   case (136) ; xcm(i) = impurity_arr(14)%frac
   !   case (137) ; xcm(i) = fplhsep
   !   case (138) ; xcm(i) = rebco_thickness
   !   case (139) ; xcm(i) = copper_thick
   !   case (140) ; xcm(i) = thkwp
   !   case (141) ; xcm(i) = fcqt
   !   case (142) ; xcm(i) = nesep
   !   case (143) ; xcm(i) = f_copperA_m2
   !   case (144) ; xcm(i) = fnesep
   !   case (145) ; xcm(i) = fgwped
   !   case (146) ; xcm(i) = fcpttf
   !   case (147) ; xcm(i) = freinke
   !   case (148) ; xcm(i) = impurity_arr(impvardiv)%frac*impurity_enrichment(impvardiv)   !fzactual
   !   case (149) ; xcm(i) = fbmaxcs
   !   case (150) ; xcm(i) = plasmod_fcdp
   !   case (151) ; xcm(i) = plasmod_fradc
   !   case (152) ; xcm(i) = fgwsep
   !   case (153) ; xcm(i) = fpdivlim
   !   case (154) ; xcm(i) = fne0

     case default
        idiags(1) = i ; idiags(2) = ixc(i)
        call report_error(54)

     end select
     

      ! Simple list of iteration variable names
      name_xc(i) = lablxc(ixc(i))
      ! Note that iteration variable 18 has more than one name:
      if ((ixc(i) == 18).and.(icurr /= 2)) name_xc(i) = 'q95'
      if ((ixc(i) == 18).and.(icurr == 2)) name_xc(i) = 'qbar'


       ! MDK Check if sweep variable is also an iteration variable
       if (name_xc(i) == vlabel) then
            write(nout,*) 'WARNING: The sweep variable is also an iteration variable.'
            write(nout,*) 'The values of the sweep variable will be overwritten by the optimiser.'
            write(*,*) 'WARNING: The sweep variable is also an iteration variable.'
       end if

     !  Check that no iteration variable is zero

     if (abs(xcm(i)) <= 1.0D-12) then
        idiags(1) = i ; idiags(2) = ixc(i)
        write(*,*) 'Iteration variable ',ixc(i),'(',trim(lablxc(ixc(i))),') is zero.'
        call report_error(55)
     end if

     !  Crude method of catching NaN errors

     !if ( (abs(xcm(i)) > 9.99D99).or.(xcm(i) /= xcm(i)) ) then
     if ( variable_error(xcm(i)) ) then
        idiags(1) = i ; idiags(2) = ixc(i) ; fdiags(1) = xcm(i)
        call report_error(56)
     end if

  end do

  do i = 1,nvar
     if (abs(xcm(i)) > 1.0D-99) then
        scale(i) = 1.0D0/xcm(i)
     else
        scale(i) = 1.0D0
     end if
     scafc(i) = 1.0D0/scale(i)
     xcm(i) = xcm(i)*scale(i)
  end do

end subroutine loadxc

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine convxc(xc,nn)

  !+ad_name  convxc
  !+ad_summ  Routine to convert scaled iteration variables back to
  !+ad_summ  their real values
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  xc(ipnvars) : input/output real array : scaled iteration variable values
  !+ad_args  nn : input integer : number of iteration variables
  !+ad_desc  This subroutine converts the scaled iteration variables back to
  !+ad_desc  their real values.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constraint_variables
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables

  !+ad_call  impurity_radiation_module
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables

  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  report_error
  !+ad_hist  14/11/11 PJK Changed NaN error check
  !+ad_hist  16/11/11 PJK Initial F90 version
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  17/01/13 PJK Removed bounds checking of iteration variables
  !+ad_hist  11/04/13 PJK Removed ti recalculation (moved to physics)
  !+ad_hist  04/06/13 PJK Added ftbr (89), blbuith (90), blbuoth (91),
  !+ad_hisc               fflutf (92), shldith (93), shldoth (94),
  !+ad_hisc               fptfnuc (95), fvvhe (96)
  !+ad_hist  19/06/13 PJK fjtfc (var.28) is obsolete - added error trap
  !+ad_hist  30/09/13 PJK Added fpsepr (97)
  !+ad_hist  28/11/13 PJK Added li6enrich (98)
  !+ad_hist  26/02/14 PJK Added ftftort (99) and ftfthko (100)
  !+ad_hist  30/04/14 PJK Added prp (101)
  !+ad_hist  19/05/14 PJK Reassigned (28) to fradpwr
  !+ad_hist  02/06/14 PJK Added fimpvar (102); special treatment required
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  01/10/14 PJK Added flhthresh (103)
  !+ad_hist  02/10/14 PJK Added fcwr (104)
  !+ad_hist  06/10/14 PJK Added fnbshinef (105)
  !+ad_hist  11/11/14 PJK Added ftmargoh (106)
  !+ad_hist  25/11/14 JM  Added favail (107)
  !+ad_hist  27/05/15 MDK Added breeder_f (108)
  !+ad_hist  05/08/15 MDK Added ralpne (109), ftaulimit (110)
  !+ad_hist  18/11/15 RK  Added fzeffmax (112)
  !+ad_hist  26/11/15 RK  Added ftaucq (113)
  !+ad_hist  10/11/16 HL  Added fradwall (116)
  !+ad_hist  19/01/17 JM  Added fpsepbqar (117)
  !+ad_hist  08/02/17 JM  Added fpsep, tesep, ttarget (118, 119, 120)
  !+ad_hist  22/02/17 JM  Added neratio (121)
  !+ad_hist  12/01/18 KE  Added fnesep (144)
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constraint_variables
  use cost_variables
  use current_drive_variables
  use define_iteration_variables 
  use divertor_kallenbach_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables

  use impurity_radiation_module
  use numerics
  use pfcoil_variables
  use physics_variables
  use plasmod_variables
  use rebco_variables
  use reinke_variables
  use tfcoil_variables
  use times_variables

  implicit none

  !  Arguments

  integer, intent(in) :: nn
  real(kind(1.0D0)), dimension(ipnvars), intent(in) :: xc

  !  Local variables

  integer :: i
  real(kind(1.0D0))::ratio

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do i = 1,nn
     ratio = xc(i)/scale(i)

     select case (ixc(i))

     case (1);  call set_itv_1(ratio)
     case (2);  call set_itv_2(ratio)
     case (3);  call set_itv_3(ratio)
     case (4);  call set_itv_4(ratio)
     case (5);  call set_itv_5(ratio)
     case (6);  call set_itv_6(ratio)
     case (7);  call set_itv_7(ratio)
     case (8);  call set_itv_8(ratio)
     case (9);  call set_itv_9(ratio)
     case (10);  call set_itv_10(ratio)
     case (11);  call set_itv_11(ratio)
     case (12);  call set_itv_12(ratio)
     case (13);  call set_itv_13(ratio)
     case (14);  call set_itv_14(ratio)
     case (15);  call set_itv_15(ratio)
     case (16);  call set_itv_16(ratio)
     case (17);  call set_itv_17(ratio)
     case (18);  call set_itv_18(ratio)
     case (19);  call set_itv_19(ratio)
     case (20);  call set_itv_20(ratio)
     case (21);  call set_itv_21(ratio)
     case (22);  
     case (23);  call set_itv_23(ratio)
     case (24);  
     case (25);  call set_itv_25(ratio)
     case (26);  call set_itv_26(ratio)
     case (27);  call set_itv_27(ratio)
     case (28);  call set_itv_28(ratio)
     case (29);  call set_itv_29(ratio)
     case (30);  call set_itv_30(ratio)
     case (31);  call set_itv_31(ratio)
     case (32);  call set_itv_32(ratio)
     case (33);  call set_itv_33(ratio)
     case (34);  call set_itv_34(ratio)
     case (35);  call set_itv_35(ratio)
     case (36);  call set_itv_36(ratio)
     case (37);  call set_itv_37(ratio)
     case (38);  call set_itv_38(ratio)
     case (39);  call set_itv_39(ratio)
     case (40);  call set_itv_40(ratio)
     case (41);  call set_itv_41(ratio)
     case (42);  call set_itv_42(ratio)
     case (43);  
     case (44);  call set_itv_44(ratio)
     case (45);  call set_itv_45(ratio)
     case (46);  call set_itv_46(ratio)
     case (47);  call set_itv_47(ratio)
     case (48);  call set_itv_48(ratio)
     case (49);  call set_itv_49(ratio)
     case (50);  call set_itv_50(ratio)
     case (51);  call set_itv_51(ratio)
     case (52);  call set_itv_52(ratio)
     case (53);  call set_itv_53(ratio)
     case (54);  call set_itv_54(ratio)
     case (55);  
     case (56);  call set_itv_56(ratio)
     case (57);  call set_itv_57(ratio)
     case (58);  call set_itv_58(ratio)
     case (59);  call set_itv_59(ratio)
     case (60);  call set_itv_60(ratio)
     case (61);  call set_itv_61(ratio)
     case (62);  call set_itv_62(ratio)
     case (63);  call set_itv_63(ratio)
     case (64);  call set_itv_64(ratio)
     case (65);  call set_itv_65(ratio)
     case (66);  call set_itv_66(ratio)
     case (67);  call set_itv_67(ratio)
     case (68);  call set_itv_68(ratio)
     case (69);  call set_itv_69(ratio)
     case (70);  call set_itv_70(ratio)
     case (71);  call set_itv_71(ratio)
     case (72);  call set_itv_72(ratio)
     case (73);  call set_itv_73(ratio)
     case (74);  call set_itv_74(ratio)
     case (75);  call set_itv_75(ratio)
     case (76);  
     case (77);  
     case (78);  
     case (79);  call set_itv_79(ratio)
     case (80);  
     case (81);  
     case (82);  
     case (83);  
     case (84);  
     case (85);  
     case (86);  
     case (87);  
     case (88);  
     case (89);  call set_itv_89(ratio)
     case (90);  call set_itv_90(ratio)
     case (91);  call set_itv_91(ratio)
     case (92);  call set_itv_92(ratio)
     case (93);  call set_itv_93(ratio)
     case (94);  call set_itv_94(ratio)
     case (95);  call set_itv_95(ratio)
     case (96);  call set_itv_96(ratio)
     case (97);  call set_itv_97(ratio)
     case (98);  call set_itv_98(ratio)
     case (99);  
     case (100);  
     case (101);  
     case (102);  call set_itv_102(ratio)
     case (103);  call set_itv_103(ratio)
     case (104);  call set_itv_104(ratio)
     case (105);  call set_itv_105(ratio)
     case (106);  call set_itv_106(ratio)
     case (107);  call set_itv_107(ratio)
     case (108);  call set_itv_108(ratio)
     case (109);  call set_itv_109(ratio)
     case (110);  call set_itv_110(ratio)
     case (111);  call set_itv_111(ratio)
     case (112);  call set_itv_112(ratio)
     case (113);  call set_itv_113(ratio)
     case (114);  call set_itv_114(ratio)
     case (115);  call set_itv_115(ratio)
     case (116);  call set_itv_116(ratio)
     case (117);  call set_itv_117(ratio)
     case (118);  call set_itv_118(ratio)
     case (119);  call set_itv_119(ratio)
     case (120);  call set_itv_120(ratio)
     case (121);  call set_itv_121(ratio)
     case (122);  call set_itv_122(ratio)
     case (123);  call set_itv_123(ratio)
     case (124);  call set_itv_124(ratio)
     case (125);  call set_itv_125(ratio)
     case (126);  call set_itv_126(ratio)
     case (127);  call set_itv_127(ratio)
     case (128);  call set_itv_128(ratio)
     case (129);  call set_itv_129(ratio)
     case (130);  call set_itv_130(ratio)
     case (131);  call set_itv_131(ratio)
     case (132);  call set_itv_132(ratio)
     case (133);  call set_itv_133(ratio)
     case (134);  call set_itv_134(ratio)
     case (135);  call set_itv_135(ratio)
     case (136);  call set_itv_136(ratio)
     case (137);  call set_itv_137(ratio)
     case (138);  call set_itv_138(ratio)
     case (139);  call set_itv_139(ratio)
     case (140);  call set_itv_140(ratio)
     case (141);  call set_itv_141(ratio)
     case (142);  call set_itv_142(ratio)
     case (143);  call set_itv_143(ratio)
     case (144);  call set_itv_144(ratio)
     case (145);  call set_itv_145(ratio)
     case (146);  call set_itv_146(ratio)
     case (147);  call set_itv_147(ratio)
     case (148);  call set_itv_148(ratio)
     case (149);  call set_itv_149(ratio)
     case (150);  call set_itv_150(ratio)
     case (151);  call set_itv_151(ratio)
     case (152);  call set_itv_152(ratio)
     case (153);  call set_itv_153(ratio)
     case (154);  call set_itv_154(ratio)
         case (155);  call set_itv_155(ratio)
         case (156);  call set_itv_156(ratio)
         case (157);  call set_itv_157(ratio)
         case (158);  call set_itv_158(ratio)
         case (159);  call set_itv_159(ratio)
         case (160);  call set_itv_160(ratio)
         case (161);  call set_itv_161(ratio)
         case (162);  call set_itv_162(ratio)
         case (163);  call set_itv_163(ratio)
         case (164);  call set_itv_164(ratio)
         case (165);  call set_itv_165(ratio)
         case (166);  call set_itv_166(ratio)
         case (167);  call set_itv_167(ratio)
         case (168);  call set_itv_168(ratio)
         case (169);  call set_itv_169(ratio)
         case (170);  call set_itv_170(ratio)      

      
   !   case (41) ; fcohbop   = xc(i)/scale(i)
   !   case (42) ; gapoh     = xc(i)/scale(i)
   !   case (43) ; write(*,*) 'Iteration variable 43 is not supported.'
   !   case (44) ; fvsbrnni  = xc(i)/scale(i)
   !   case (45) ; fqval     = xc(i)/scale(i)
   !   case (46) ; fpinj     = xc(i)/scale(i)
   !   case (47) ; feffcd    = xc(i)/scale(i)
   !   case (48) ; fstrcase  = xc(i)/scale(i)
   !   case (49) ; fstrcond  = xc(i)/scale(i)
   !   case (50) ; fiooic    = xc(i)/scale(i)
   !   case (51) ; fvdump    = xc(i)/scale(i)
   !   case (52) ; vdalw     = xc(i)/scale(i)
   !   case (53) ; fjprot    = xc(i)/scale(i)
   !   case (54) ; ftmargtf  = xc(i)/scale(i)
   !   case (55) ; write(*,*) 'Iteration variable 55 is not supported.'
   !   case (56) ; tdmptf    = xc(i)/scale(i)
   !   case (57) ; thkcas    = xc(i)/scale(i)
   !   case (58) ; thwcndut  = xc(i)/scale(i)
   !   case (59) ; fcutfsu   = xc(i)/scale(i)
   !   case (60) ; cpttf     = xc(i)/scale(i)
   !   case (61) ; gapds     = xc(i)/scale(i)
   !   case (62) ; fdtmp     = xc(i)/scale(i)
   !   case (63) ; ftpeak    = xc(i)/scale(i)
   !   case (64) ; fauxmn    = xc(i)/scale(i)
   !   case (65) ; tohs      = xc(i)/scale(i)
   !   case (66) ; ftohs     = xc(i)/scale(i)
   !   case (67) ; ftcycl    = xc(i)/scale(i)
   !   case (68) ; fptemp    = xc(i)/scale(i)
   !   case (69) ; rcool     = xc(i)/scale(i)
   !   case (70) ; vcool     = xc(i)/scale(i)
   !   case (71) ; fq        = xc(i)/scale(i)
   !   case (72) ; fipir     = xc(i)/scale(i)
   !   case (73) ; scrapli   = xc(i)/scale(i)
   !   case (74) ; scraplo   = xc(i)/scale(i)
   !   case (75) ; tfootfi   = xc(i)/scale(i)
   !   case (76) ; write(*,*) 'Iteration variable 76 is not supported.'
   !   case (77) ; write(*,*) 'Iteration variable 77 is not supported.'
   !   case (78) ; write(*,*) 'Iteration variable 78 is not supported.'
   !   case (79) ; fbetap    = xc(i)/scale(i)
   !   case (80) ; write(*,*) 'Iteration variable 80 is not supported.'
   !   case (81) ; write(*,*) 'Iteration variable 81 is not supported.'
   !   case (82) ; write(*,*) 'Iteration variable 82 is not supported.'
   !   case (83) ; write(*,*) 'Iteration variable 83 is not supported.'
   !   case (84) ; write(*,*) 'Iteration variable 84 is not supported.'
   !   case (85) ; write(*,*) 'Iteration variable 85 is not supported.'
   !   case (86) ; write(*,*) 'Iteration variable 86 is not supported.'
   !   case (87) ; write(*,*) 'Iteration variable 87 is not supported.'
   !   case (88) ; write(*,*) 'Iteration variable 88 is not supported.'
   !   case (89) ; ftbr      = xc(i)/scale(i)
   !   case (90) ; blbuith   = xc(i)/scale(i)
   !   case (91) ; blbuoth   = xc(i)/scale(i)
   !   case (92) ; fflutf    = xc(i)/scale(i)
   !   case (93) ; shldith   = xc(i)/scale(i)
   !   case (94) ; shldoth   = xc(i)/scale(i)
   !   case (95) ; fptfnuc   = xc(i)/scale(i)
   !   case (96) ; fvvhe     = xc(i)/scale(i)
   !   case (97) ; fpsepr    = xc(i)/scale(i)
   !   case (98) ; li6enrich = xc(i)/scale(i)
   !   case (99) ; write(*,*) 'Iteration variable 99 is not supported.'
   !   case (100); write(*,*) 'Iteration variable 100 is not supported.'
   !   case (101); write(*,*) 'Iteration variable 101 is not supported.'
   !   case (102)
   !      fimpvar = xc(i)/scale(i)
   !      impurity_arr(impvar)%frac = fimpvar
   !   case (103) ; flhthresh = xc(i)/scale(i)
   !   case (104) ; fcwr      = xc(i)/scale(i)
   !   case (105) ; fnbshinef = xc(i)/scale(i)
   !   case (106) ; ftmargoh  = xc(i)/scale(i)
   !   case (107) ; favail    = xc(i)/scale(i)
   !   case (108) ; breeder_f = xc(i)/scale(i)
   !   case (109) ; ralpne    = xc(i)/scale(i)
   !   case (110) ; ftaulimit = xc(i)/scale(i)
   !   case (111) ; fniterpump = xc(i)/scale(i)
   !   case (112) ; fzeffmax = xc(i)/scale(i)
   !   case (113) ; ftaucq = xc(i)/scale(i)
   !   case (114) ; fw_channel_length = xc(i)/scale(i)
   !   case (115) ; fpoloidalpower = xc(i)/scale(i)
   !   case (116) ; fradwall = xc(i)/scale(i)
   !   case (117) ; fpsepbqar = xc(i)/scale(i)
   !   case (118) ; fpsep = xc(i)/scale(i)
   !   case (119) ; tesep = xc(i)/scale(i)
   !   case (120) ; ttarget = xc(i)/scale(i)
   !   case (121) ; neratio = xc(i)/scale(i)
   !   case (122) ; oh_steel_frac = xc(i)/scale(i)
   !   case (123) ; foh_stress = xc(i)/scale(i)
   !   case (124) ; qtargettotal = xc(i)/scale(i)
   !   case (125) ; impurity_arr(3)%frac = xc(i)/scale(i)
   !   case (126) ; impurity_arr(4)%frac = xc(i)/scale(i)
   !   case (127) ; impurity_arr(5)%frac = xc(i)/scale(i)
   !   case (128) ; impurity_arr(6)%frac = xc(i)/scale(i)
   !   case (129) ; impurity_arr(7)%frac = xc(i)/scale(i)
   !   case (130) ; impurity_arr(8)%frac = xc(i)/scale(i)
   !   case (131) ; impurity_arr(9)%frac = xc(i)/scale(i)
   !   case (132) ; impurity_arr(10)%frac = xc(i)/scale(i)
   !   case (133) ; impurity_arr(11)%frac = xc(i)/scale(i)
   !   case (134) ; impurity_arr(12)%frac = xc(i)/scale(i)
   !   case (135) ; impurity_arr(13)%frac = xc(i)/scale(i)
   !   case (136) ; impurity_arr(14)%frac = xc(i)/scale(i)
   !   case (137) ; fplhsep = xc(i)/scale(i)
   !   case (138) ; rebco_thickness = xc(i)/scale(i)
   !   case (139) ; copper_thick = xc(i)/scale(i)
   !   case (140) ; thkwp = xc(i)/scale(i)
   !   case (141) ; fcqt = xc(i)/scale(i)
   !   case (142) ; nesep = xc(i)/scale(i)
   !   case (143) ; f_copperA_m2 = xc(i)/scale(i)
   !   case (144) ; fnesep = xc(i)/scale(i)
   !   case (145) ; fgwped = xc(i)/scale(i)
   !   case (146) ; fcpttf = xc(i)/scale(i)
   !   case (147) ; freinke = xc(i)/scale(i)
   !   case (148)
   !      fzactual = xc(i)/scale(i)
   !      impurity_arr(impvardiv)%frac = fzactual / impurity_enrichment(impvardiv)
   !      write(*,*) 'fzactual = ', fzactual
   !   case (149) ; fbmaxcs = xc(i)/scale(i)
   !   case (150) ; plasmod_fcdp = xc(i)/scale(i)
   !   case (151) ; plasmod_fradc = xc(i)/scale(i)
   !   case (152) ; fgwsep = xc(i)/scale(i)
   !   case (153) ; fpdivlim = xc(i)/scale(i)
   !   case (154) ; fne0 = xc(i)/scale(i)

     case default

        call report_error(57)

     end select

     !  Check that no iteration variable is zero

     if (abs(xc(i)) <= 1.0D-12) then
        idiags(1) = i ; idiags(2) = ixc(i)
        write(*,*) 'Iteration variable ',ixc(i),'(',trim(lablxc(ixc(i))),') is zero.'
        call report_error(58)
     end if

     !  Crude method of catching NaN errors

     !if ((abs(xc(i)) > 9.99D99).or.(xc(i) /= xc(i))) then
     if(variable_error(xc(i)))then
        idiags(1) = i ; idiags(2) = ixc(i) ; fdiags(1) = xc(i)
        call report_error(59)
     end if

     if (abs(scale(i)) < 1.0D-99) then
        idiags(1) = i ; idiags(2) = ixc(i)
        call report_error(60)
     end if

  end do

end subroutine convxc

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine boundxc

  !+ad_name  boundxc
  !+ad_summ  Routine to convert variable bounds to their real values
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This subroutine converts the scaled iteration variable bounds
  !+ad_desc  back to their real values.
  !+ad_prob  None
  !+ad_call  numerics
  !+ad_hist  22/10/92 PJK Removed arguments (xc,nn)
  !+ad_hist  16/11/11 PJK Initial F90 version
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use numerics

  implicit none

  !  Local variables

  integer :: i

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do i = 1,nvar
     bondl(i) = boundl(ixc(i))*scale(i)
     bondu(i) = boundu(ixc(i))*scale(i)
  end do

end subroutine boundxc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
