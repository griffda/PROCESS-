! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module buildings_module
  !! author: J. Morris, P. Knight, R. Chapman (UKAEA)
  !!
  !! This module contains routines for calculating the
  !! parameters of the fusion power plant buildings.
  !!

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

contains

  subroutine bldgcall(outfile, iprint)
    !! author: J. Morris, P. Knight, R. Chapman (UKAEA)
    !!
    !! This routine calls the buildings calculations.

  !!!!!!!!!!! ************ !!!!!!!!!!!!!!!!!!!
    ! use build_variables, only: r_tf_inboard_mid, r_tf_outboard_mid, tfthko, tfcth, &
    !   hmax, rsldo, d_vv_top, d_vv_bot, vgap2, rsldi
    ! use fwbs_variables, only: whtshld, rdewex
    ! use buildings_variables, only: cryvol, volrci, rbvol, rmbvol, wsvol, elevol
    ! use heat_transport_variables, only: helpow
    ! use pfcoil_variables, only: pfrmax, pfmmax
    ! use tfcoil_variables, only: whttf, n_tf

    use build_variables, only: r_tf_inboard_mid, r_tf_outboard_mid, tfcth, tfthko, &
      hmax
    use pfcoil_variables, only: pfrmax
    use fwbs_variables, only: rdewex
    !use buildings_variables, only: cryvol, volrci, rbvol, rmbvol, wsvol, elevol
    !use heat_transport_variables, only: helpow    
    !use tfcoil_variables, only: whttf, n_tf

    implicit none

    !  Arguments 

    integer, intent(in) :: iprint
    !! switch for writing to output file (1=yes)

    integer, intent(in) :: outfile
    !! output file unit

    !  Local variables

    real(8) :: tf_radial_dim
    !! Radial dimension of TF coil (m)

    real(8) :: tf_vertical_dim
    !! Vertical dimension of TF coil (m)

    ! real(8) :: tfh
    ! real(8) :: tfmtn
    ! real(8) :: tfri
    ! real(8) :: tfro


    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! Find width, in radial dimension, of TF coil (m)
    !  = (outboard mid-leg radial position + half-thickness of outboard leg) - 
    !    (inboard mid-leg radial position - half-thickness of inboard leg)
    tf_radial_dim = r_tf_outboard_mid + ( tfthko * 0.5D0 ) - &
                    r_tf_inboard_mid - ( tfcth * 0.5D0 )

    ! Find full height of TF coil (m)
    !  = 2 * (mid-plane to TF coil inside edge + thickness of coil)
    tf_vertical_dim = 2.0D0 * ( hmax + tfthko )


    ! tfh = (hmax + tfcth)*2.0D0
    ! !! TF coil vertical height (m)
    ! ! Rem : SK not valid for single null

  !   ! Reactor vault wall and roof thicknesses are hardwired
  !   call bldgs(pfrmax,pfmmax,tfro,tfri,tfh,tfmtn,n_tf,rsldo, &
  !     rsldi,2.0D0*(hmax-vgap2)-d_vv_top-d_vv_bot,whtshld, &
  !     rdewex,helpow,iprint,outfile,cryvol,volrci,rbvol,rmbvol, &
  !     wsvol,elevol)
  ! !!!!!!!!!!! ************ !!!!!!!!!!!!!!!!!!!

    ! Calculate building areas and volumes
    call bldgs_sizes(pfrmax, rdewex, tf_radial_dim, tf_vertical_dim, &
    outfile, iprint)

  end subroutine bldgcall

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bldgs_sizes(pfrmax, rdewex, tf_radial_dim, tf_vertical_dim, &
    outfile, iprint)

    !! Subroutine that estimates the sizes (areas and volumes) of 
    !! buildings within a fusion power plant.
    !! Some estimates are scaled with parameters of the fusion plant,
    !! some are derived from footprints/volumes based on 
    !! assessment of other power plants and/or similar facilities.
    !!
    !! author: R Chapman, UKAEA
    !!
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use buildings_variables, only: reactor_wall_thk, reactor_roof_thk, reactor_fndtn_thk, &
      reactor_clrnc, transp_clrnc, cryostat_clrnc, ground_clrnc, &
      crane_clrnc_h, crane_clrnc_v, crane_arm_h, &
      reactor_hall_l, reactor_hall_w, reactor_hall_h, &
      warm_shop_l, warm_shop_w, warm_shop_h, &
      turbine_hall_l, turbine_hall_w, turbine_hall_h, &
      gas_buildings_l, gas_buildings_w, gas_buildings_h, &
      water_buildings_l, water_buildings_w, water_buildings_h, &
      sec_buildings_l, sec_buildings_w, sec_buildings_h
    use cost_variables, only: tlife
    use constants, only: pi
    use process_output, only: oheadr, ovarre

    implicit none

    !  Arguments 
    
    real(8), intent(in) :: pfrmax
    !! radius of largest PF coil (m)
    real(8), intent(in) :: rdewex
    !! cryostat radius (m)
    real(8), intent(in) :: tf_radial_dim
    !! Radial dimension of TF coil (m)
    real(8), intent(in) :: tf_vertical_dim
    !! Vertical dimension of TF coil (m)

    integer, intent(in) :: outfile, iprint

    !  Local variables

    real(8) :: width_reactor_piece
    !! radial width of largest reactor component (m)
    real(8) :: half_width
    !! half-width of reactor building (m)
    real(8) :: height_clrnc
    !! vertical clearance required in reactor building (m)

    real(8) :: reactor_hall_area
    !! footprint of reactor hall (m2)
    real(8) :: reactor_hall_vol
    !! volume of reactor hall (m3)
    real(8) :: reactor_hall_area_ext
    !! footprint of reactor hall, including walls (m2)
    real(8) :: reactor_hall_vol_ext
    !! volume of reactor hall, including walls, roof, foundation (m3)
    real(8) :: reactor_basement_l
    !! reactor length (m)
    real(8) :: reactor_basement_w
    !! reactor width (m)
    real(8) :: reactor_basement_h
    !! reactor height (m)
    real(8) :: reactor_basement_area
    !! footprint of reactor basement (m2)
    real(8) :: reactor_basement_vol
    !! volume of reactor basement (m3)
    real(8) :: reactor_building_vol
    !! volume of reactor hall + basement (m3)

    real(8) :: warm_shop_area
    !! area of warm shop (m2)
    real(8) :: warm_shop_vol
    !! volume of warm shop (m3)

    real(8) :: turbine_hall_area
    !! footprint of turbine hall (m2)
    real(8) :: turbine_hall_vol
    !! volume of turbine hall (m3)

    real(8) :: gas_buildings_area
    !! footprint of air & gas supply buildings (m2)
    real(8) :: gas_buildings_vol
    !! volume of air & gas supply buildings (m3)    
    real(8) :: water_buildings_area
    !! footprint of water, laundry & drainage buildings (m2)
    real(8) :: water_buildings_vol
    !! volume of water, laundry & drainage buildings (m3)
    real(8) :: sec_buildings_area
    !! footprint of security & safety buildings (m2)
    real(8) :: sec_buildings_vol
    !! volume of security & safety buildings (m3)

    real(8) :: staff_buildings_area 
    !! footprint of staff buildings (m2)
    real(8) :: staff_buildings_vol
    !! volume of staff buildings (m3)

    !! footprint of  buildings (m2)
    !! volume of  buildings (m3)

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Reactor building

    ! Lateral size driven by radial width of largest component, from:
    !  PF coil max radius, cryostat radius, TF coil outer radius
    width_reactor_piece = max(pfrmax, rdewex, tf_radial_dim)

    ! Calculate half-width of building (m)
    ! include radial width of largest component *twice*, to allow for construction;
    ! include clearance around reactor, transportation clearance between components,
    ! clearance to building wall for crane operation
    half_width = (2.0D0 * width_reactor_piece) + reactor_clrnc + transp_clrnc + crane_clrnc_h

    ! Width of reactor building
    reactor_hall_w = 2.0D0 * half_width
    
    ! Length of reactor building found from width + half-width:
    ! allows for laydown of large components during construction
    reactor_hall_l = reactor_hall_w + half_width
    
    ! Calculate vertical clearance required (above and below reactor):
    ! include clearance around reactor, transportation clearance between components,
    ! clearance from TF coil to cryostat, clearance beneath TF coil,
    ! clearance to ceiling for crane operation, crane arm height
    height_clrnc = reactor_clrnc + transp_clrnc &
                  + cryostat_clrnc + ground_clrnc &
                  + crane_clrnc_h + crane_arm_h

    ! Height of reactor building
    ! include height of TF coil *twice*, to allow for construction/maintenance
    reactor_hall_h = (2.0D0 * tf_vertical_dim) + height_clrnc             
   
    ! Reactor building internal footprint and volume
    reactor_hall_area = reactor_hall_l * reactor_hall_w
    reactor_hall_vol = reactor_hall_area * reactor_hall_h

    ! Reactor building external footprint and volume
    reactor_hall_area_ext = (reactor_hall_l + 2.0D0*reactor_wall_thk) &
                           * (reactor_hall_w + 2.0D0*reactor_wall_thk)

    reactor_hall_vol_ext = reactor_hall_area_ext * &
                          (reactor_hall_h + reactor_roof_thk + reactor_fndtn_thk) 
    

    ! Reactor maintenance basement and tunnel

    ! Architecture proposed here is a basement directly beneath the reactor enabling the
    ! downwards extraction of hot components. The footprint estimated here is oversized to 
    ! include allowance for a tunnel to the hot cell storage/maintenance building.
    reactor_basement_l = reactor_hall_w
    reactor_basement_w = reactor_hall_w
    reactor_basement_area = reactor_basement_l * reactor_basement_w

    ! basement height still includes some clearances
    reactor_basement_h = tf_vertical_dim + transp_clrnc + crane_clrnc_h + crane_arm_h

    reactor_basement_vol = reactor_basement_area * reactor_basement_h
    
    ! Calculate external volume of reactor hall + maintenace basement and tunnel
    reactor_building_vol = reactor_hall_vol_ext + reactor_basement_vol


 
    
    ! reactor auxiliary
    !**********************************************************

    ! power
    !**********************************************************

    ! control
    !**********************************************************

    ! decon, hot cell, warm shop
    !derived from reactor size... 
    
    ! ! Hot Buildings

    ! ! Hot Storage
    ! ! A simplification of R. Gowland's estimates of Operational Active Waste Storage,
    ! ! this assumes all IVC components used through the life of the plant will need storage.
    ! ! The storage area required is derived from the sizes and number of components, allowing
    ! ! for a margin in component numbers as set by the quantity safety factor (qnty_sfty_fac).

    
    ! hot_store_building_area = 0.0D0


        
    ! ! outboard first wall + shield + blanket
    
    ! ! number of components = num of modules in poloidal direction * num of modules in toroidal direction
    ! components_in_build = nblktmodpo * nblktmodto

    ! ! number of components required through lifetime of power plant, given an 
    ! ! estimated lifetime of the component; rounded up to an integer
    ! components_thru_life = ceiling((tlife / fwbllife) * components_in_build * qnty_sfty_fac)
    
    ! ! radial dimension of component = thickness
    ! dim1 = fwoth + blnkoth + shldoth
    ! ! vertical dimension of component = (2 * plasma half-height) / (num poloidal components)
    ! dim2 = 2.0D0 * (rminor * kappa) / nblktmodpo
    ! ! toroidal dimension of component = (circumference at radius of component) / (num toroidal components)
    ! dim3 = 2*pi*(rminor+rmajor+scraplo+fwoth+blnkoth+shldoth) / nblktmodto

    ! ! for storage, allow separation distance between parts
    ! dim1 = dim1 + sepdist
    ! dim2 = dim2 + sepdist
    ! dim3 = dim3 + sepdist

    ! ! find storage volume required per component, including separation distance
    ! compt_store_vol = dim1 * dim2 * dim3
    ! ! find storage volume required for total components through lifetime of plant
    ! compt_store_vol = comp_store_vol * components_thru_life

    
    ! hot_store_building_h = 12.0D0
    ! ! find footprint required, given set height of building (from RG's estimates)
    ! compt_store_area = compt_store_vol / hot_store_building_h

    ! ! if area was square, length and width would be:
    ! sq_len = sqrt(compt_store_area)

    ! ! length including separation to walls and hot cell walls; set width to match
    ! compt_store_l = sq_len + (2.0D0 * sepdist) + (2.0D0 * hotcell_wall_thk)
    ! compt_store_w = compt_store_l

    ! compt_store_area = compt_store_l * compt_store_w
    ! compt_store_vol = compt_store_area * hot_store_building_h
    


    ! step22010301 = step_a22010301()

    ! function step_a22010301() result(step22010301)
    !   !! 22.01.03.01 TF Coils
    !   !! Returns cost of TF coils
    !   use cost_variables, only: step_ref, cpstcst, ifueltyp, step_uc_cryo_al, &
    !     step_mc_cryo_al_per, uccpcl1, uccpclb
    !   use tfcoil_variables, only: i_tf_sup, whtconal, n_tf, whttflgs, whtcp
    !   use physics_variables, only: itart
    !   implicit none
  
    !   ! Result
    !   real(dp) :: step22010301
    !   !! Cost of TF coils in M$

    ! end function step_a22010301
  

    
    ! ! 
    ! Maintenance


    ! number of hot cells, their size and functions can only sensibly be determined once the failure modes and recovery process (if any)  of each IVC has been identified.


    ! ! Reactor building internal footprint and volume
    ! reactor_hall_area
    ! reactor_hall_vol

    ! ! Reactor building external footprint and volume
    ! reactor_hall_area_ext

    ! reactor_hall_vol_ext


    ! Warm Shop
    ! Values taken from W. Smith's estimates of necessary facilities (and related sizes):
    !  'hands on maintenance workshops for low RA dose equipment'
    warm_shop_area = warm_shop_l * warm_shop_w
    warm_shop_vol = warm_shop_area * warm_shop_h


    ! maintenance
    !**********************************************************

    ! cryo & cooling
    !**********************************************************

    ! electrical
    !**********************************************************
  
    ! Turbine Hall
    ! As proposed by R. Gowland, based on assessment of 18 existing fission power plants: 
    ! turbine hall size is largely independent of plant output power. 
    ! The default footprint used here represents a weighted mean of those plants 
    ! and the design of a Steam Rankine cycle turbine building suitable for STEP, 
    ! produced by Morsons as part of the Year 1 work.
    turbine_hall_area = turbine_hall_l * turbine_hall_w
    turbine_hall_vol = turbine_hall_area * turbine_hall_h

    ! waste
    !**********************************************************

    ! Site Services
    ! Derived from W. Smith's estimates of necessary facilities and their sizes; 
    ! buildings grouped by function.
    !
    ! Air & Gas supplies
    ! Includes compressed air facility, common gas systems facility, bottled gas 
    ! storage compounds; these values amalgamate multiple individual buildings.
    gas_buildings_area = gas_buildings_l * gas_buildings_w
    gas_buildings_vol = gas_buildings_area * gas_buildings_h
    !
    ! Water, Laundry & Drainage
    ! Includes facilities for potable water, firewater, chilled water; PPE laundry & 
    ! Respiratory Protective Equipment cleaning; industrial drains & sewage 
    ! process and discharge; these values amalgamate multiple individual buildings.
    water_buildings_area = water_buildings_l * water_buildings_w
    water_buildings_vol = water_buildings_area * water_buildings_h
    !
    ! Site Security & Safety
    ! Includes Security Control Centre and Fire and Ambulance Garages; 
    ! these values amalgamate multiple individual buildings.
    sec_buildings_area = sec_buildings_l * sec_buildings_w
    sec_buildings_vol = sec_buildings_area * sec_buildings_h

    ! Staff Services
    ! Derived from W. Smith's estimates of necessary facilities and their sizes; 
    ! includes main office buildings, contractor offices, staff restaurant and cafe,
    ! staff induction and training facilities, main gate and reception, access control
    ! and site pass office, occupational health centre.
    ! These values amalgamate the estimates of floor area for all individual buildings,
    ! and use an average building height of 5m.
    staff_buildings_area = 5.0D5
    staff_buildings_vol = staff_buildings_area * 5.0D0
    
    ! Output    
    if (iprint == 0) return
    call oheadr(outfile,'Plant Buildings System - RMC') ! rmc
    call ovarre(outfile,'reactor_hall_l (m)', '(reactor_hall_l)', reactor_hall_l)
    call ovarre(outfile,'reactor_hall_w (m)', '(reactor_hall_w)', reactor_hall_w)
    call ovarre(outfile,'reactor_hall_h (m)', '(reactor_hall_h)', reactor_hall_h)
    call ovarre(outfile,'Internal footprint of Reactor Hall (m2)', '(reactor_hall_area)', reactor_hall_area)
    call ovarre(outfile,'Internal volume of Reactor Hall (m3)', '(reactor_hall_vol)', reactor_hall_vol)
    call ovarre(outfile,'External footprint of Reactor Hall (m2)', '(reactor_hall_area_ext)', reactor_hall_area_ext)
    call ovarre(outfile,'External volume of Reactor Hall (m3)', '(reactor_hall_vol_ext)', reactor_hall_vol_ext)
    call ovarre(outfile,'Footprint of Reactor Basement (m2)', '(reactor_basement_area)', reactor_basement_area) ! RMC check
    call ovarre(outfile,'Volume of Reactor Basement (m3)', '(reactor_basement_vol)', reactor_basement_vol)
    call ovarre(outfile,'Volume of Reactor Hall + Basement (m3)', '(reactor_building_vol)', reactor_building_vol)

    call ovarre(outfile,'warm_shop_l (m)', '(warm_shop_l)', warm_shop_l)
    call ovarre(outfile,'warm_shop_w (m)', '(warm_shop_w)', warm_shop_w)
    call ovarre(outfile,'warm_shop_h (m)', '(warm_shop_h)', warm_shop_h)
    call ovarre(outfile,'Footprint of Warm Shop (m2)', '(warm_shop_area)', warm_shop_area)
    call ovarre(outfile,'Volume of Warm Shop (m2)', '(warm_shop_vol)', warm_shop_vol)

    call ovarre(outfile,'turbine_hall_l (m)', '(turbine_hall_l)', turbine_hall_l)
    call ovarre(outfile,'turbine_hall_w (m)', '(turbine_hall_w)', turbine_hall_w)
    call ovarre(outfile,'turbine_hall_h (m)', '(turbine_hall_h)', turbine_hall_h)
    call ovarre(outfile,'Footprint of Turbine Hall (m2)', '(turbine_hall_area)', turbine_hall_area)
    call ovarre(outfile,'Volume of Turbine Hall (m3)', '(turbine_hall_vol)', turbine_hall_vol)

    call ovarre(outfile,'Footprint of air & gas supply buildings (m2)', '(gas_buildings_area)', gas_buildings_area)
    call ovarre(outfile,'Volume of air & gas supply buildings (m3)', '(gas_buildings_vol)', gas_buildings_vol)
    call ovarre(outfile,'Footprint of water supply buildings (m2)', '(water_buildings_area)', water_buildings_area)
    call ovarre(outfile,'Volume of water supply buildings (m3)', '(water_buildings_vol)', water_buildings_vol)
    call ovarre(outfile,'Footprint of Security & Safety buildings (m2)', '(sec_buildings_area)', sec_buildings_area)
    call ovarre(outfile,'Volume of Security & Safety buildings (m3)', '(sec_buildings_vol)', sec_buildings_vol)
    call ovarre(outfile,'Footprint of staff buildings (m2)', '(staff_buildings_area)', staff_buildings_area)
    call ovarre(outfile,'Volume of staff buildings (m3)', '(staff_buildings_vol)', staff_buildings_vol)

    !call ovarre(outfile,' (m)', '()', )
    !call ovarre(outfile,'Footprint of  buildings (m2)', '(_buildings_area)', _buildings_area)
    !call ovarre(outfile,'Volume of  buildings (m3)', '(_buildings_vol)', _buildings_vol)


    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




    ! ! Maintenance building
    ! ! The reactor maintenance building includes the hot cells, the
    ! ! decontamination chamber, the transfer corridors, and the waste
    ! ! treatment building.  The dimensions of these areas are scaled
    ! ! from a reference design based on the shield sector size.

    ! ! Transport corridor size
    ! ! hcwt : hot cell wall thickness, m
    ! tcw = shro-shri + 4.0D0*trcl
    ! tcl = 5.0D0*tcw + 2.0D0*hcwt

    ! ! Decontamination cell size
    ! dcw = 2.0D0*tcw + 1.0D0
    ! dcl = 2.0D0*tcw + 1.0D0

    ! ! Hot cell size
    ! ! hccl : clearance around components in hot cell, m
    ! hcw = shro-shri + 3.0D0*hccl + 2.0D0
    ! hcl = 3.0D0*(shro-shri) + 4.0D0*hccl + tcw

    ! ! Radioactive waste treatment
    ! rww = dcw
    ! rwl = hcl - dcl - hcwt

    ! ! Maintenance building dimensions
    ! rmbw = hcw + dcw + 3.0D0*hcwt
    ! rmbl = hcl + 2.0D0*hcwt

    ! ! Height
    ! ! wgt2 : hot cell crane capacity (kg)
    ! !        Calculated if 0 is input
    ! if (wgt2 >  1.0D0) then
    !    wgts = wgt2
    ! else
    !    wgts = shmf*shm/n_tf
    ! end if
    ! cran = 9.41D-6*wgts + 5.1D0
    ! rmbh = 10.0D0 + shh + trcl + cran + stcl + fndt
    ! tch = shh + stcl + fndt

    ! ! Volume
    ! rmbv = mbvfac * rmbw*rmbl*rmbh + tcw*tcl*tch

    ! ! Warm shop and hot cell gallery
    ! wsa = (rmbw+7.0D0)*20.0D0 + rmbl*7.0D0
    ! wsv = wsvfac * wsa*rmbh

    ! ! Cryogenic building volume
    ! cryv = 55.0D0 * sqrt(helpow)

    ! ! Other building volumes
    ! ! pibv : power injection building volume, m3
    ! ! esbldgm3 is forced to be zero if no energy storage is required (lpulse=0)
    ! elev = tfcbv + pfbldgm3 + esbldgm3 + pibv

    ! ! Calculate effective floor area for ac power module
    ! efloor = (rbv+rmbv+wsv+triv+elev+conv+cryv+admv+shov)/6.0D0
    ! admvol = admv
    ! shovol = shov
    ! convol = conv

    ! ! Total volume of nuclear buildings
    ! volnucb = ( vrci + rmbv + wsv + triv + cryv )




  end subroutine bldgs_sizes

end module buildings_module

! subroutine bldgs(pfr,pfm,tfro,tfri,tfh,tfm,n_tf,shro,shri, &
!   shh,shm,crr,helpow,iprint,outfile,cryv,vrci,rbv,rmbv,wsv,elev)
!   !! Determines the sizes of the plant buildings
!   !! author: P J Knight, CCFE, Culham Science Centre
!   !! author: P C Shipe, ORNL
!   !! pfr : input/output real :  largest PF coil outer radius, m
!   !! pfm : : input real : largest PF coil mass, tonne
!   !! tfro : input real : outer radius of TF coil, m
!   !! tfri : input real : inner radius of TF coil, m
!   !! tfh : input real : full height of TF coil, m
!   !! tfm : input real : mass of one TF coil, tonne
!   !! tfno : input real : number of tf coils
!   !! shro : input real : outer radius of attached shield, m
!   !! shri : input real : inner radius of attached shield, m
!   !! shh : input real : height of attached shield, m
!   !! shm : input real : total mass of attached shield, kg
!   !! crr : input real : outer radius of common cryostat, m
!   !! helpow : input real : total cryogenic load, W
!   !! iprint : input integer : switch for writing to output file (1=yes)
!   !! outfile : input integer : output file unit
!   !! cryv : output real : volume of cryogenic building, m3
!   !! vrci : output real : inner volume of reactor building, m3
!   !! rbv : output real : outer volume of reactor building, m3
!   !! rmbv : output real : volume of reactor maintenance building, m3
!   !! wsv : output real : volume of warm shop, m3
!   !! elev : output real : volume of electrical buildings, m3
!   !! This routine determines the size of the plant buildings.
!   !! The reactor building and maintenance building are sized
!   !! based on the tokamak dimensions. The cryogenic building volume is
!   !! scaled based on the total cryogenic load. The other building
!   !! sizes are input from other modules or by the user.
!   !! This routine was modified to include fudge factors (fac1,2,...)
!   !! to fit the ITER design, September 1990 (J. Galambos).
!   !! This routine was included in PROCESS in January 1992 by
!   !! P. C. Shipe.
!   !! None
!   !
!   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!   use buildings_variables, only: wrbi, rxcl, trcl, row, wgt, shmf, clh2, clh1, &
!     stcl, rbvfac, rbwt, rbrt, fndt, hcwt, hccl, wgt2, mbvfac, wsvfac, &
!     tfcbv, pfbldgm3, esbldgm3, pibv, efloor, admvol, triv, conv, admv, shov, &
!     shovol, convol, volnucb
!   use process_output, only: oheadr, ovarre

!   implicit none

!   ! Arguments
!   integer, intent(in) :: iprint, outfile
!   real(dp), intent(inout) :: pfr
!   real(dp), intent(in) :: pfm,tfro,tfri,tfh,tfm,n_tf,shro, &
!        shri,shh,shm,crr,helpow

!   real(dp), intent(out) :: cryv,vrci,rbv,rmbv,wsv,elev

!   ! Local variables !
!   ! !!!!!!!!!!!!!!!!!!

!   real(dp) :: ang, bmr, coill, crcl, cran, dcl,dcw, drbi, &
!        hcl, hcw, hrbi, hy, layl, rbh, rbl, rbw, rmbh, rmbl, rmbw, rwl, rww, &
!        sectl, tch, tcl, tcw, wgts, wsa, wt

!   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!   ! Reactor building

!   ! Determine basic machine radius (m)
!   ! crr  :  cryostat radius (m)
!   ! pfr  :  radius of largest PF coil (m)
!   ! tfro :  outer radius of TF coil (m)
!   bmr = max(crr,pfr,tfro)

!   ! Determine largest transported piece
!   sectl = shro - shri  ! Shield thicknes (m)
!   coill = tfro - tfri  ! TF coil thickness (m)
!   sectl = max(coill, sectl)

!   ! Calculate half width of building (m)
!   ! rxcl : clearance around reactor, m
!   ! trcl : transportation clearance between components, m
!   ! row  : clearance to building wall for crane operation, m
!   wrbi = bmr + rxcl + sectl + trcl + row

!   ! Calculate length to allow PF or cryostat laydown (m)

!   ! Laydown length (m)
!   layl = max(crr,pfr)

!   ! Diagnoal length (m)
!   hy = bmr + rxcl + sectl + trcl + layl

!   ! Angle between diagnoal length and floor (m)
!   ang = (wrbi-trcl-layl)/hy

!   ! Cap angle at 1
!   if (abs(ang) > 1.0D0) then
!     ang = abs(ang)/ang
!   end if

!   ! Length to allow laydown (m)
!   drbi = trcl + layl + hy*sin(acos(ang)) + wrbi

!   ! Crane height based on maximum lift (m)
!   ! wgt : reactor building crane capacity (kg)
!   !       Calculated if 0 is input
!   ! shmf : fraction of shield mass per TF coil to be moved in
!   !        the maximum shield lift
!   if (wgt > 1.0D0) then
!      wt = wgt
!   else
!      wt = shmf*shm/n_tf
!      wt = max(wt, 1.0D3*pfm, 1.0D3*tfm)
!   end if

!   ! Crane height (m)
!   crcl = 9.41D-6*wt + 5.1D0

!   ! Building height (m)
!   ! clh1 : clearance from TF coil to cryostat top, m
!   ! clh2 : clearance beneath TF coil to foundation, including basement, m
!   ! stcl : clearance above crane to roof, m
!   ! Additional tfh allows TF coil to be lifted right out
!   hrbi = clh2 + 2.0D0*tfh + clh1 + trcl + crcl + stcl

!   ! Internal volume (m3)
!   vrci = rbvfac * 2.0D0*wrbi*drbi*hrbi

!   ! External dimensions of reactor building (m)
!   ! rbwt : reactor building wall thickness, m
!   ! rbrt : reactor building roof thickness, m
!   ! fndt : foundation thickness, m
!   rbw = 2.0D0*wrbi + 2.0D0*rbwt
!   rbl = drbi + 2.0D0*rbwt
!   rbh = hrbi + rbrt + fndt
!   rbv = rbvfac * rbw*rbl*rbh

!   ! Maintenance building
!   ! The reactor maintenance building includes the hot cells, the
!   ! decontamination chamber, the transfer corridors, and the waste
!   ! treatment building.  The dimensions of these areas are scaled
!   ! from a reference design based on the shield sector size.

!   ! Transport corridor size
!   ! hcwt : hot cell wall thickness, m
!   tcw = shro-shri + 4.0D0*trcl
!   tcl = 5.0D0*tcw + 2.0D0*hcwt

!   ! Decontamination cell size
!   dcw = 2.0D0*tcw + 1.0D0
!   dcl = 2.0D0*tcw + 1.0D0

!   ! Hot cell size
!   ! hccl : clearance around components in hot cell, m
!   hcw = shro-shri + 3.0D0*hccl + 2.0D0
!   hcl = 3.0D0*(shro-shri) + 4.0D0*hccl + tcw

!   ! Radioactive waste treatment
!   rww = dcw
!   rwl = hcl - dcl - hcwt

!   ! Maintenance building dimensions
!   rmbw = hcw + dcw + 3.0D0*hcwt
!   rmbl = hcl + 2.0D0*hcwt

!   ! Height
!   ! wgt2 : hot cell crane capacity (kg)
!   !        Calculated if 0 is input
!   if (wgt2 >  1.0D0) then
!      wgts = wgt2
!   else
!      wgts = shmf*shm/n_tf
!   end if
!   cran = 9.41D-6*wgts + 5.1D0
!   rmbh = 10.0D0 + shh + trcl + cran + stcl + fndt
!   tch = shh + stcl + fndt

!   ! Volume
!   rmbv = mbvfac * rmbw*rmbl*rmbh + tcw*tcl*tch

!   ! Warm shop and hot cell gallery
!   wsa = (rmbw+7.0D0)*20.0D0 + rmbl*7.0D0
!   wsv = wsvfac * wsa*rmbh

!   ! Cryogenic building volume
!   cryv = 55.0D0 * sqrt(helpow)

!   ! Other building volumes
!   ! pibv : power injection building volume, m3
!   ! esbldgm3 is forced to be zero if no energy storage is required (lpulse=0)
!   elev = tfcbv + pfbldgm3 + esbldgm3 + pibv

!   ! Calculate effective floor area for ac power module
!   efloor = (rbv+rmbv+wsv+triv+elev+conv+cryv+admv+shov)/6.0D0
!   admvol = admv
!   shovol = shov
!   convol = conv

!   ! Total volume of nuclear buildings
!   volnucb = ( vrci + rmbv + wsv + triv + cryv )

!   ! Output !
!   ! !!!!!!!!!
  
!   if (iprint == 0) return
!   call oheadr(outfile,'Plant Buildings System')
!   call ovarre(outfile,'Internal volume of reactor building (m3)', '(vrci)', vrci)
!   call ovarre(outfile,'Dist from centre of torus to bldg wall (m)', '(wrbi)', wrbi)
!   call ovarre(outfile,'Effective floor area (m2)','(efloor)',efloor)
!   call ovarre(outfile,'Reactor building volume (m3)','(rbv)',rbv)
!   call ovarre(outfile,'Reactor maintenance building volume (m3)', '(rmbv)', rmbv)
!   call ovarre(outfile,'Warmshop volume (m3)','(wsv)',wsv)
!   call ovarre(outfile,'Tritium building volume (m3)','(triv)',triv)
!   call ovarre(outfile,'Electrical building volume (m3)','(elev)',elev)
!   call ovarre(outfile,'Control building volume (m3)','(conv)',conv)
!   call ovarre(outfile,'Cryogenics building volume (m3)','(cryv)',cryv)
!   call ovarre(outfile,'Administration building volume (m3)','(admv)',admv)
!   call ovarre(outfile,'Shops volume (m3)','(shov)',shov)
!   call ovarre(outfile,'Total volume of nuclear buildings (m3)', '(volnucb)', volnucb)

! end subroutine bldgs
