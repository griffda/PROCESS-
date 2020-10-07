! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module build_module

  !! Module containing machine build routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! geometry (radial and vertical builds) of the fusion power
  !! plant core.


  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  private
  public :: radialb, vbuild, portsz

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine radialb(outfile,iprint)

    !! Radial build
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: R Kemp, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This subroutine determines the radial build of the machine.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use numerics, only: ixc, nvar
    use build_variables, only: sigallpc, r_tf_outboard_mid, vgap2, ddwex, &
      shldlth, tftsgap, dr_tf_inner_bore, blnkith, thshield, rsldi, blnkoth, &
      rsldo, tfcth, tfthko, vgaptop, blnktth, gapsto, vgap, vvblgap, &
      r_vv_inboard_out, fwareaob, tfoffset, shldtth, rbld, iprecomp, &
      r_tf_inboard_mid, r_tf_inboard_in, shldtth, blbuith, r_vv_inboard_out, &
      tfcth, gapsto, vgaptop, precomp, r_tf_inboard_out, gapomin, vvblgap, &
      fwareaob, blnktth, rbld, blnkoth, tfoffset, iprecomp, plsepo, tfthko, &
      rsldo, vgap, gapoh, fwoth, ohcth, shldoth, scraplo, fwith, blbpith, &
      tfootfi, blbuoth, gapds, fwareaib, fseppc, scrapli, blbmith, shldith, &
      ddwi, fwarea, blbpoth, blbmoth, fcspc, bore, r_cp_top, r_sh_inboard_out, &
      r_sh_inboard_in, f_r_cp, i_r_cp_top
    use constants, only: mfile, nout, pi
    use current_drive_variables, only: beamwd
    use divertor_variables, only: divfix
    use error_handling, only: idiags, fdiags, report_error
    use fwbs_variables, only: fwbsshape, blktmodel, fhcd, fdiv
    use maths_library, only: eshellarea, dshellarea
    use pfcoil_variables, only: ohhghf
    use physics_variables, only: itart, i_single_null, idivrt, kappa, triang, &
      rminor, rmajor
    use process_output, only: ocmmnt, oheadr, ovarre, ovarin, obuild, oblnkl
    use tfcoil_variables, only: ripple, tinstf, wwp1, drtop, i_tf_sup, n_tf, &
      dr_tf_wp, ripmax, thkcas, tfinsgap, casthi
    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables


    real(dp) :: hbot,hfw,htop,r1,r2,r3,radius,r_tf_outboard_midl,vbuild, vbuild1

    real(dp) :: fwtth

    integer :: ripflag = 0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate total blanket thicknesses if blktmodel > 0
    if ( blktmodel > 0 ) then
       blnkith = blbuith + blbmith + blbpith
       blnkoth = blbuoth + blbmoth + blbpoth
       shldtth = 0.5D0*(shldith+shldoth)
    end if

    !  Top/bottom blanket thickness
    blnktth = 0.5D0*(blnkith+blnkoth)

    !  Check if vgaptop has been set too small
    vgaptop = max(0.5d0*(scrapli+scraplo), vgaptop)

    ! Calculate pre-compression structure thickness is iprecomp=1
    if (iprecomp == 1) then
      precomp = fseppc / (2.0d0 * pi * fcspc * sigallpc * (bore + bore + ohcth))
    else
      precomp = 0.0D0
    end if

    ! Inboard side inner radius [m]
    r_tf_inboard_in = bore + ohcth + precomp + gapoh

    ! Issue #514 Radial dimensions of inboard leg
    ! Calculate tfcth if dr_tf_wp is an iteration variable (140)
    if ( any(ixc(1:nvar) == 140) ) then
    
        ! SC TF coil thickness defined using its maximum (diagonal)
        if ( i_tf_sup == 1 ) then
           tfcth = ( r_tf_inboard_in + dr_tf_wp + casthi + thkcas ) / cos(pi/n_tf) &
                 - r_tf_inboard_in

         ! Rounded resistive TF geometry
        else
            tfcth = dr_tf_wp + casthi + thkcas
        end if

    end if

    ! Radial build to tfcoil middle [m]
    r_tf_inboard_mid = r_tf_inboard_in + 0.5D0*tfcth

    ! Radial build to tfcoil plasma facing side [m]
    r_tf_inboard_out = r_tf_inboard_in + tfcth

    ! Radius of the centrepost at the top of the machine
    if ( itart == 1 ) then
    
       ! r_cp_top is set using the plasma shape
       if ( i_r_cp_top == 0 ) then      
          r_cp_top = rmajor - rminor * triang - ( tftsgap + thshield + shldith + &
                     vvblgap + blnkith + fwith +  3.0D0*scrapli ) + drtop
          r_cp_top = max( r_cp_top, r_tf_inboard_out * 1.01D0 ) 

          ! Top and bottom TF CP radius ratio
          f_r_cp = r_cp_top / r_tf_inboard_out

       ! User defined r_cp_top
       else if ( i_r_cp_top == 1 ) then     
       
          ! Error if if r_cp_top is larger than the top plasma radius + shields
          if ( r_cp_top > rmajor - rminor * triang - ( tftsgap + thshield +& 
               shldith + vvblgap + blnkith + fwith +  3.0D0*scrapli ) + drtop ) then

             fdiags(1) = r_cp_top
             call report_error(256)
          end if

          ! Top and bottom TF CP radius ratio
          f_r_cp = r_cp_top / r_tf_inboard_out
         
       ! r_cp_top set as a fraction of the outer TF midplane radius
       else if ( i_r_cp_top == 2 ) then 
         r_cp_top = f_r_cp * r_tf_inboard_out        
       end if
    end if ! End of itart == 1

    !  Radial position of vacuum vessel [m]
    r_vv_inboard_out = r_tf_inboard_out + tftsgap + thshield + gapds + ddwi

    ! Radial position of the inner side of inboard neutronic shield [m]
    r_sh_inboard_in = r_vv_inboard_out

    ! Radial position of the plasma facing side of inboard neutronic shield [m]
    r_sh_inboard_out = r_sh_inboard_in + shldith

    !  Radial build to centre of plasma (should be equal to rmajor)
    rbld = r_sh_inboard_out + vvblgap + blnkith + fwith + scrapli + rminor

    !  Radius to inner edge of inboard shield
    rsldi = rmajor - rminor - scrapli - fwith - blnkith - shldith

    !  Radius to outer edge of outboard shield
    rsldo = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth

    !  Thickness of outboard TF coil legs
    if ( i_tf_sup /= 1 ) then
       tfthko = tfootfi*tfcth
    else
       tfthko = tfcth
    end if

    !  Radius to centre of outboard TF coil legs
    r_tf_outboard_mid = rsldo + vvblgap + ddwi + gapomin + thshield + tftsgap + 0.5D0*tfthko

    ! TF coil horizontal bore [m]
    dr_tf_inner_bore = ( r_tf_outboard_mid - 0.5D0*tfthko ) - ( r_tf_inboard_mid - 0.5D0*tfcth )

    call ripple_amplitude(ripple,ripmax,r_tf_outboard_mid,r_tf_outboard_midl,ripflag)

    !  If the ripple is too large then move the outboard TF coil leg
    if (r_tf_outboard_midl > r_tf_outboard_mid) then
       r_tf_outboard_mid = r_tf_outboard_midl
       gapsto = r_tf_outboard_mid - 0.5D0*tfthko - ddwi - rsldo - thshield - tftsgap - vvblgap
       dr_tf_inner_bore = ( r_tf_outboard_mid - 0.5D0*tfthko ) - ( r_tf_inboard_mid - 0.5D0*tfcth )
    else
       gapsto = gapomin
    end if

    !  Call ripple calculation again with new r_tf_outboard_mid/gapsto value
    !  call rippl(ripmax,rmajor,rminor,r_tf_outboard_mid,n_tf,ripple,r_tf_outboard_midl)
    call ripple_amplitude(ripple,ripmax,r_tf_outboard_mid,r_tf_outboard_midl,ripflag)

    !  Calculate first wall area
    !  Old calculation... includes a mysterious factor 0.875
    !fwarea = 0.875D0 * &
    !     ( 4.0D0*pi**2*sf*rmajor*(rminor+0.5D0*(scrapli+scraplo)) )

    !  Half-height of first wall (internal surface)

    hbot = rminor*kappa + vgap + divfix - blnktth - 0.5D0*(fwith+fwoth)
    if (idivrt == 2) then  !  (i.e. i_single_null=0)
       htop = hbot
    else
       htop = rminor*kappa + vgaptop
    end if
    hfw = 0.5D0*(htop + hbot)

    if ((itart == 1).or.(fwbsshape == 1)) then  !  D-shaped

       !  Major radius to outer edge of inboard section
       r1 = rmajor - rminor - scrapli

       !  Horizontal distance between inside edges,
       !  i.e. outer radius of inboard part to inner radius of outboard part

       r2 = (rmajor + rminor + scraplo) - r1

       !  Calculate surface area, assuming 100% coverage
       call dshellarea(r1,r2,hfw,fwareaib,fwareaob,fwarea)

    else  !  Cross-section is assumed to be defined by two ellipses

       !  Major radius to centre of inboard and outboard ellipses
       !  (coincident in radius with top of plasma)

       r1 = rmajor - rminor*triang

       !  Distance between r1 and outer edge of inboard section

       r2 = r1 - (rmajor - rminor - scrapli)

       !  Distance between r1 and inner edge of outboard section

       r3 = (rmajor + rminor + scraplo) - r1

       !  Calculate surface area, assuming 100% coverage

       call eshellarea(r1,r2,r3,hfw,fwareaib,fwareaob,fwarea)

    end if

    !  Apply area coverage factor

    if (idivrt == 2) then
      ! Double null configuration
      fwareaob = fwareaob*(1.0D0-2.0D0*fdiv-fhcd)
      fwareaib = fwareaib*(1.0D0-2.0D0*fdiv-fhcd)
    else
      ! Single null configuration 
      fwareaob = fwareaob*(1.0D0-fdiv-fhcd)
      fwareaib = fwareaib*(1.0D0-fdiv-fhcd)
    end if 

    fwarea = fwareaib + fwareaob

    if (fwareaob <= 0.0D0) then
       fdiags(1) = fdiv ; fdiags(2) = fhcd
       call report_error(61)
    end if

    !end if

    if (iprint == 0) return

    !  Print out device build

    call oheadr(outfile,'Radial Build')

    if (ripflag /= 0) then
       call ocmmnt(outfile, &
            '(Ripple result may not be accurate, as the fit was outside')
       call ocmmnt(outfile, &
            ' its range of applicability.)')
       call oblnkl(outfile)
       call report_error(62)

       if (ripflag == 1) then
          fdiags(1) = wwp1*n_tf/rmajor
          call report_error(141)
       else if (ripflag == 2) then
          ! Convert to integer as idiags is integer array
          idiags(1) = INT(n_tf)
          call report_error(142)
       else
          fdiags(1) = (rmajor+rminor)/r_tf_outboard_mid
          call report_error(143)
       end if
    end if

    write(outfile,10)
   10  format(t43,'Thickness (m)',t60,'Radius (m)')

    radius = 0.0D0
    call obuild(outfile,'Device centreline',0.0D0,radius)

    radius = radius + bore
    call obuild(outfile,'Machine bore',bore,radius,'(bore)')
    call ovarre(mfile,'Machine bore (m)','(bore)',bore)
    
    radius = radius + ohcth
    call obuild(outfile,'Central solenoid',ohcth,radius,'(ohcth)')
    call ovarre(mfile,'CS radial thickness (m)','(ohcth)',ohcth)
    
    radius = radius + precomp
    call obuild(outfile,'CS precompression',precomp,radius,'(precomp)')
    call ovarre(mfile,'CS precompression (m)','(precomp)',precomp)
    
    radius = radius + gapoh
    call obuild(outfile,'Gap',gapoh,radius,'(gapoh)')
    call ovarre(mfile,'CS precompresion to TF coil radial gap (m)','(gapoh)',gapoh)
    
    radius = radius + tfcth
    call obuild(outfile,'TF coil inboard leg',tfcth,radius,'(tfcth)')
    call ovarre(mfile,'TF coil inboard leg (m)','(tfcth)',tfcth)
    
    radius = radius + tftsgap
    call obuild(outfile,'Gap',tftsgap,radius,'(tftsgap)')
    call ovarre(mfile,'TF coil inboard leg insulation gap (m)','(tftsgap)',tftsgap)

    radius = radius + thshield
    call obuild(outfile,'Thermal shield',thshield,radius,'(thshield)')
    call ovarre(mfile,'Thermal shield (m)','(thshield)',thshield)

    radius = radius + gapds
    call obuild(outfile,'Gap',gapds,radius,'(gapds)')
    call ovarre(mfile,'thermal shield to vessel radial gap (m)','(gapds)',gapds)

    radius = radius + ddwi + shldith
    call obuild(outfile,'Vacuum vessel (and shielding)',ddwi + shldith,radius,'(ddwi + shldith)')
    call ovarre(mfile,'Vacuum vessel radial thickness (m)','(ddwi)',ddwi)
    call ovarre(mfile,'Inner radiation shield radial thickness (m)','(shldith)',shldith)

    radius = radius + vvblgap
    call obuild(outfile,'Gap',vvblgap,radius,'(vvblgap)')
    call ovarre(mfile,'Gap (m)','(vvblgap)',vvblgap)

    radius = radius + blnkith
    call obuild(outfile,'Inboard blanket',blnkith,radius,'(blnkith)')
    call ovarre(mfile,'Inboard blanket radial thickness (m)','(blnkith)',blnkith)

    radius = radius + fwith
    call obuild(outfile,'Inboard first wall',fwith,radius,'(fwith)')
    call ovarre(mfile,'Inboard first wall radial thickness (m)','(fwith)',fwith)

    radius = radius + scrapli
    call obuild(outfile,'Inboard scrape-off',scrapli,radius,'(scrapli)')
    call ovarre(mfile,'Inboard scrape-off radial thickness (m)','(scrapli)',scrapli)

    radius = radius + rminor
    call obuild(outfile,'Plasma geometric centre',rminor,radius,'(rminor)')

    radius = radius + rminor
    call obuild(outfile,'Plasma outboard edge',rminor,radius,'(rminor)')

    radius = radius + scraplo
    call obuild(outfile,'Outboard scrape-off',scraplo,radius,'(scraplo)')
    call ovarre(mfile,'Outboard scrape-off radial thickness (m)','(scraplo)',scraplo)

    radius = radius + fwoth
    call obuild(outfile,'Outboard first wall',fwoth,radius,'(fwoth)')
    call ovarre(mfile,'Outboard first wall radial thickness (m)','(fwoth)',fwoth)

    radius = radius + blnkoth
    call obuild(outfile,'Outboard blanket',blnkoth,radius,'(blnkoth)')
    call ovarre(mfile,'Outboard blanket radial thickness (m)','(blnkoth)',blnkoth)

    radius = radius + vvblgap
    call obuild(outfile,'Gap',vvblgap,radius,'(vvblgap)')

    radius = radius + ddwi+shldoth
    call obuild(outfile,'Vacuum vessel (and shielding)',ddwi+shldoth,radius,'(ddwi+shldoth)')
    call ovarre(mfile,'Outer radiation shield radial thickness (m)','(shldoth)',shldoth)

    radius = radius + gapsto
    call obuild(outfile,'Gap',gapsto,radius,'(gapsto)')
    call ovarre(mfile,'Vessel to TF radial gap (m)','(gapsto)',gapsto)

    radius = radius + thshield
    call obuild(outfile,'Thermal shield',thshield,radius,'(thshield)')

    radius = radius + tftsgap
    call obuild(outfile,'Gap',tftsgap,radius,'(tftsgap)')
    call ovarre(mfile,'Gap (m)','(tftsgap)',tftsgap)

    radius = radius + tfthko
    call obuild(outfile,'TF coil outboard leg',tfthko,radius,'(tfthko)')
    call ovarre(mfile,'TF coil outboard leg radial thickness (m)','(tfthko)',tfthko)

    !  Vertical build

    call oheadr(outfile,'Vertical Build')

    call ovarin(mfile,'Divertor null switch','(i_single_null)',i_single_null)

    if (i_single_null == 0) then
       call ocmmnt(outfile,'Double null case')

       write(outfile,20)
       20 format(t43,'Thickness (m)',t60,'Height (m)')

       vbuild = 0.0D0
       call obuild(outfile,'Midplane',0.0D0,vbuild)

       vbuild = vbuild + rminor * kappa
       call obuild(outfile,'Plasma top',rminor*kappa,vbuild,'(rminor*kappa)')
       call ovarre(mfile,'Plasma half-height (m)','(rminor*kappa)',rminor*kappa)

       vbuild = vbuild + vgap
       call obuild(outfile,'Top scrape-off',vgap,vbuild,'(vgap)')
       call ovarre(mfile,'Top scrape-off vertical thickness (m)','(vgap)',vgap)

       vbuild = vbuild + divfix
       call obuild(outfile,'Divertor structure',divfix,vbuild,'(divfix)')
       call ovarre(mfile,'Divertor structure vertical thickness (m)','(divfix)',divfix)

       vbuild = vbuild + shldlth + ddwi
       call obuild(outfile,'Vacuum vessel (and shielding)',ddwi+shldlth,vbuild,'(ddwi+shldlth)')
       call ovarre(mfile,'Bottom radiation shield thickness (m)','(shldlth)',shldlth)

       vbuild = vbuild + vgap2
       call obuild(outfile,'Gap',vgap2,vbuild,'(vgap2)')
       call ovarre(mfile,'Vessel - TF coil vertical gap (m)','(vgap2)',vgap2)

       vbuild = vbuild + thshield
       call obuild(outfile,'Thermal shield',thshield,vbuild,'(thshield)')

       vbuild = vbuild + tftsgap
       call obuild(outfile,'Gap',tftsgap,vbuild,'(tftsgap)')

       vbuild = vbuild + tfcth
       call obuild(outfile,'TF coil',tfcth,vbuild,'(tfcth)')

       ! end of Double null case
    else
       call ocmmnt(outfile,'Single null case')
       write(outfile,20)

       vbuild = tfcth + tftsgap + thshield + vgap2 + ddwi + vvblgap + shldtth + blnktth + &
            0.5D0*(fwith+fwoth) + vgaptop + rminor*kappa

       ! To calculate vertical offset between TF coil centre and plasma centre
       vbuild1 = vbuild

       call obuild(outfile,'TF coil',tfcth,vbuild,'(tfcth)')
       vbuild = vbuild - tfcth

       call obuild(outfile,'Gap',tftsgap,vbuild,'(tftsgap)')
       vbuild = vbuild - tftsgap

       call obuild(outfile,'Thermal shield',thshield,vbuild,'(thshield)')
       vbuild = vbuild - thshield

       call obuild(outfile,'Gap',vgap2,vbuild,'(vgap2)')
       call ovarre(mfile,'Vessel - TF coil vertical gap (m)','(vgap2)',vgap2)
       vbuild = vbuild - vgap2

       call obuild(outfile,'Vacuum vessel (and shielding)',ddwi+shldtth,vbuild,'(ddwi+shldtth)')
       vbuild = vbuild - ddwi - shldtth
       call ovarre(mfile,'Top radiation shield thickness (m)','(shldtth)',shldtth)

       call obuild(outfile,'Gap',vvblgap,vbuild,'(vvblgap)')
       vbuild = vbuild - vvblgap

       call obuild(outfile,'Top blanket',blnktth,vbuild,'(blnktth)')
       call ovarre(mfile,'Top blanket vertical thickness (m)','(blnktth)',blnktth)
       vbuild = vbuild - blnktth

       fwtth = 0.5D0*(fwith+fwoth)
       call obuild(outfile,'Top first wall',fwtth,vbuild,'(fwtth)')
       call ovarre(mfile,'Top first wall vertical thickness (m)', 'fwtth',fwtth)
       vbuild = vbuild - fwtth

       call obuild(outfile,'Top scrape-off',vgaptop,vbuild,'(vgaptop)')
       call ovarre(mfile,'Top scrape-off vertical thickness (m)', 'vgaptop', vgaptop)
       vbuild = vbuild - vgaptop

       call obuild(outfile,'Plasma top',rminor*kappa,vbuild,'(rminor*kappa)')
       call ovarre(mfile,'Plasma half-height (m)','(rminor*kappa)',rminor*kappa)
       vbuild = vbuild - rminor*kappa

       call obuild(outfile,'Midplane',0.0D0,vbuild)

       vbuild = vbuild - rminor*kappa
       call obuild(outfile,'Plasma bottom',rminor*kappa,vbuild,'(rminor*kappa)')

       vbuild = vbuild - vgap
       call obuild(nout,'Lower scrape-off',vgap,vbuild,'(vgap)')
       call ovarre(mfile,'Bottom scrape-off vertical thickness (m)','(vgap)',vgap)

       vbuild = vbuild - divfix
       call obuild(outfile,'Divertor structure',divfix,vbuild,'(divfix)')
       call ovarre(mfile,'Divertor structure vertical thickness (m)', '(divfix)',divfix)

       vbuild = vbuild - shldlth

       vbuild = vbuild - ddwi
       call obuild(nout,'Vacuum vessel (and shielding)',ddwi+shldlth,vbuild,'(ddwi+shldlth)')
       call ovarre(mfile,'Bottom radiation shield thickness (m)','(shldlth)',shldlth)

       vbuild = vbuild - vgap2
       call obuild(nout,'Gap',vgap2,vbuild,'(vgap2)')

       vbuild = vbuild - thshield
       call obuild(outfile,'Thermal shield',thshield,vbuild,'(thshield)')
       
       vbuild = vbuild - tftsgap
       call obuild(outfile,'Gap',tftsgap,vbuild,'(tftsgap)')
       
       vbuild = vbuild - tfcth
       call obuild(nout,'TF coil',tfcth,vbuild,'(tfcth)')

       ! To calculate vertical offset between TF coil centre and plasma centre
       tfoffset = (vbuild1 + vbuild) / 2.0d0

       ! end of Single null case
    end if

    !  Other build quantities

    call ovarre(mfile,'External cryostat thickness (m)','(ddwex)',ddwex)
    call ovarre(mfile,'Ratio of Central solenoid height to TF coil internal height', &
         '(ohhghf)',ohhghf)
    call ovarre(mfile,'Width of neutral beam duct where it passes between the TF coils (m)', &
         '(beamwd)',beamwd)

  end subroutine radialb

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vbuild(outfile,iprint)

    !! Vertical build
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: R Kemp, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This subroutine determines the vertical build of the machine
    !! inside the TF coil.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use build_variables, only: shldtth, vgap2, fwoth, vgap, vvblgap, hpfdif, &
      tfcth, vgaptop, hpfu, thshield, fwith, tftsgap, dh_tf_inner_bore, &
      shldlth, hmax, blnktth, ddwi
		use divertor_variables, only: divfix
		use physics_variables, only: rminor, i_single_null, kappa
    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(dp) :: divht

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate the divertor geometry

    call divgeom(divht, outfile, iprint)
    ! Issue #481 Remove vgaptf
    if (vgap < 0.00001D0) then
       vgap = divht
    end if
    ! If vgap /= 0 use the value set by the user.

    ! Height to inside edge of TF coil
    ! Rem SK : definition only valid for double null! 
    hmax = rminor*kappa + vgap + divfix + shldlth + ddwi + vgap2 + thshield + tftsgap

    ! TF coil vertical bore [m] (Not sure it is entirely consistent !)
    ! Rem SK : not consistend for single null!
    dh_tf_inner_bore = 2.0D0*(rminor*kappa + vgaptop + fwith + blnktth + vvblgap + &
        shldtth + ddwi+ vgap2 + thshield + tftsgap)

    !  Vertical locations of divertor coils
    if (i_single_null == 0) then
       hpfu = hmax + tfcth
       hpfdif = 0.0D0
    else
       hpfu = tfcth + tftsgap + thshield + vgap2 + ddwi + shldtth + vvblgap + blnktth + &
            0.5D0*(fwith+fwoth) + vgaptop + rminor*kappa
       hpfdif = (hpfu - (hmax+tfcth)) / 2.0D0
    end if

  end subroutine vbuild

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine divgeom(divht,outfile,iprint)

    !! Divertor geometry calculation
    !! author: J Galambos, ORNL
    !! author: P J Knight, CCFE, Culham Science Centre
    !! divht : output real : divertor height (m)
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This subroutine determines the divertor geometry.
    !! The inboard (i) and outboard (o) plasma surfaces
    !! are approximated by arcs, and followed past the X-point to
    !! determine the maximum height.
    !! TART option: Peng SOFT paper
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use build_variables, only: rspo, plleno, tfoffset, plsepi, plleni, plsepo
		use divertor_variables, only: betao, betai
		use physics_variables, only: itart, rmajor, rminor, idivrt, kappa, triang
		use process_output, only: ocmmnt, oblnkl, ovarrf, oheadr
    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    real(dp), intent(out) :: divht

    !  Local variables

    !real(dp), parameter :: soleno = 0.2D0  !  length along outboard divertor
    !  plate that scrapeoff hits
    real(dp) :: kap,thetao, rci, rco, thetai
    ! real(dp) :: yspointo,xspointo,yprimeb,xpointo, tri, rprimeo, phio
    ! real(dp) :: denomo, alphad
    real(dp) :: triu, tril, rxpt, zxpt
    real(dp) :: rspi, zspi, zspo, rplti, zplti
    real(dp) :: rplbi, zplbi, rplto, zplto, rplbo, zplbo
    real(dp) :: ptop_radial,ptop_vertical

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  TART option with expanded divertor chamber

    if (itart == 1) then
       divht = 1.75D0 * rminor
       return
    end if

    !  Conventional tokamak divertor model
    !  options for seperate upper and lower triangularity

    kap = kappa
    triu = triang
    tril = triang

    !! Old method: assumes that divertor arms are continuations of arcs
    !
    !!  Outboard side
    !!  plsepo = poloidal length along the separatrix from null to
    !!           strike point on outboard [default 1.5 m]
    !!  thetao = arc angle between the strike point and the null point
    !
    !xpointo = rmajor + 0.5D0*rminor*(kap**2 + tri**2 - 1.0D0) / &
    !     (1.0D0 - tri)
    !rprimeo = (xpointo - rmajor + rminor)
    !phio = asin(kap*rminor/rprimeo)
    !thetao = plsepo/rprimeo
    !
    !!  Initial strike point
    !
    !yspointo = rprimeo * sin(thetao + phio)
    !xspointo = xpointo - rprimeo * cos(thetao + phio)
    !
    !!  Outboard strike point radius - normalized to ITER
    !
    !rstrko = xspointo + 0.14D0
    !
    !!  Uppermost divertor strike point (end of power decay)
    !!  anginc = angle of incidence of scrape-off field lines on the
    !!           divertor (rad)
    !
    !!+**PJK 25/07/11 Changed sign of anginc contribution
    !yprimeb = soleno * cos(thetao + phio - anginc)
    !
    !divht = yprimeb + yspointo - kap*rminor

    ! New method, assuming straight legs -- superceded by new method 26/5/2016
    ! Assumed 90 degrees at X-pt -- wrong!
    !
    !  Find half-angle of outboard arc
    !denomo = (tril**2 + kap**2 - 1.0D0)/( 2.0D0*(1.0D0+tril) ) - tril
    !thetao = atan(kap/denomo)
    !!  Angle between horizontal and inner divertor leg
    !alphad = (pi/2.0d0) - thetao

    ! Method 26/05/2016
    ! Find radius of inner and outer plasma arcs

    rco = 0.5*sqrt((rminor**2 * ((tril+1.0d0)**2 + kap**2)**2)/((tril + 1.0d0)**2))
    rci = 0.5*sqrt((rminor**2 * ((tril-1.0d0)**2 + kap**2)**2)/((tril - 1.0d0)**2))

    ! Find angles between vertical and legs
    ! Inboard arc angle = outboard leg angle

    thetao = asin(1.0d0 - (rminor*(1.0d0 - tril))/rci)

    ! Outboard arc angle = inboard leg angle

    thetai = asin(1.0d0 - (rminor*(1.0d0 + tril))/rco)

    !  Position of lower x-pt
    rxpt = rmajor - tril*rminor
    zxpt = -1.0d0 * kap * rminor

    ! Position of inner strike point
    !rspi = rxpt - plsepi*cos(alphad)
    !zspi = zxpt - plsepi*sin(alphad)
    rspi = rxpt - plsepi*cos(thetai)
    zspi = zxpt - plsepi*sin(thetai)

    ! Position of outer strike point
    !rspo = rxpt + plsepo*cos((pi/2.0d0)-alphad)
    !zspo = zxpt - plsepo*sin((pi/2.0d0)-alphad)
    rspo = rxpt + plsepo*cos(thetao)
    zspo = zxpt - plsepo*sin(thetao)

    ! Position of inner plate ends
    !rplti = rspi - (plleni/2.0d0)*sin(betai + alphad - pi/2.0d0)
    !zplti = zspi + (plleni/2.0d0)*cos(betai + alphad - pi/2.0d0)
    !rplbi = rspi + (plleni/2.0d0)*sin(betai + alphad - pi/2.0d0)
    !zplbi = zspi - (plleni/2.0d0)*cos(betai + alphad - pi/2.0d0)
    rplti = rspi + (plleni/2.0d0)*cos(thetai + betai)
    zplti = zspi + (plleni/2.0d0)*sin(thetai + betai)
    rplbi = rspi - (plleni/2.0d0)*cos(thetai + betai)
    zplbi = zspi - (plleni/2.0d0)*sin(thetai + betai)

    ! Position of outer plate ends
    !rplto = rspo + (plleno/2.0d0)*sin(betao - alphad)
    !zplto = zspo + (plleno/2.0d0)*cos(betao - alphad)
    !rplbo = rspo - (plleno/2.0d0)*sin(betao - alphad)
    !zplbo = zspo - (plleno/2.0d0)*cos(betao - alphad)
    rplto = rspo - (plleno/2.0d0)*cos(thetao + betao)
    zplto = zspo + (plleno/2.0d0)*sin(thetao + betao)
    rplbo = rspo + (plleno/2.0d0)*cos(thetao + betao)
    zplbo = zspo - (plleno/2.0d0)*sin(thetao + betao)

    divht = max(zplti, zplto) - min(zplbo, zplbi)

    if (iprint == 1) then
      if (idivrt == 1) then
         call oheadr(outfile, 'Divertor build and plasma position')
         call ocmmnt(outfile, 'Divertor Configuration = Single Null Divertor')
         call oblnkl(outfile)         
         ptop_radial = rmajor - triu*rminor
         ptop_vertical = kap*rminor
         call ovarrf(outfile, 'Plasma top position, radial (m)', '(ptop_radial)', ptop_radial, 'OP ')
         call ovarrf(outfile, 'Plasma top position, vertical (m)', '(ptop_vertical)', ptop_vertical, 'OP ')
         call ovarrf(outfile, 'Plasma geometric centre, radial (m)', '(rmajor.)', rmajor, 'OP ')
         call ovarrf(outfile, 'Plasma geometric centre, vertical (m)', '(0.0)', 0.0d0, 'OP ')
         call ovarrf(outfile, 'Plasma lower triangularity', '(tril)', tril, 'OP ')
         call ovarrf(outfile, 'Plasma elongation', '(kappa.)', kap, 'OP ')
         call ovarrf(outfile, 'TF coil vertical offset (m)', '(tfoffset)', tfoffset, 'OP ')
         call ovarrf(outfile, 'Plasma outer arc radius of curvature (m)', '(rco)', rco, 'OP ')
         call ovarrf(outfile, 'Plasma inner arc radius of curvature (m)', '(rci)', rci, 'OP ')
         call ovarrf(outfile, 'Plasma lower X-pt, radial (m)', '(rxpt)', rxpt, 'OP ')
         call ovarrf(outfile, 'Plasma lower X-pt, vertical (m)', '(zxpt)', zxpt, 'OP ')
         call ovarrf(outfile, 'Poloidal plane angle between vertical and inner leg (rad)', '(thetai)', thetai, 'OP ')
         call ovarrf(outfile, 'Poloidal plane angle between vertical and outer leg (rad)', '(thetao)', thetao, 'OP ')
         call ovarrf(outfile, 'Poloidal plane angle between inner leg and plate (rad)', '(betai)', betai)
         call ovarrf(outfile, 'Poloidal plane angle between outer leg and plate (rad)', '(betao)', betao)
         call ovarrf(outfile, 'Inner divertor leg poloidal length (m)', '(plsepi)', plsepi)
         call ovarrf(outfile, 'Outer divertor leg poloidal length (m)', '(plsepo)', plsepo)
         call ovarrf(outfile, 'Inner divertor plate length (m)', '(plleni)', plleni)
         call ovarrf(outfile, 'Outer divertor plate length (m)', '(plleno)', plleno)
         call ovarrf(outfile, 'Inner strike point, radial (m)', '(rspi)', rspi, 'OP ')
         call ovarrf(outfile, 'Inner strike point, vertical (m)', '(zspi)', zspi, 'OP ')
         call ovarrf(outfile, 'Inner plate top, radial (m)', '(rplti)', rplti, 'OP ')
         call ovarrf(outfile, 'Inner plate top, vertical (m)', '(zplti)', zplti, 'OP ')
         call ovarrf(outfile, 'Inner plate bottom, radial (m)', '(rplbi)', rplbi, 'OP ')
         call ovarrf(outfile, 'Inner plate bottom, vertical (m)', '(zplbi)', zplbi, 'OP ')
         call ovarrf(outfile, 'Outer strike point, radial (m)', '(rspo)', rspo, 'OP ')
         call ovarrf(outfile, 'Outer strike point, vertical (m)', '(zspo)', zspo, 'OP ')
         call ovarrf(outfile, 'Outer plate top, radial (m)', '(rplto)', rplto, 'OP ')
         call ovarrf(outfile, 'Outer plate top, vertical (m)', '(zplto)', zplto, 'OP ')
         call ovarrf(outfile, 'Outer plate bottom, radial (m)', '(rplbo)', rplbo, 'OP ')
         call ovarrf(outfile, 'Outer plate bottom, vertical (m)', '(zplbo)', zplbo, 'OP ')
         call ovarrf(outfile, 'Calculated maximum divertor height (m)', '(divht)', divht, 'OP ')
   
      else if (idivrt == 2) then
         call oheadr(outfile, 'Divertor build and plasma position')
         call ocmmnt(outfile, 'Divertor Configuration = Double Null Divertor')
         call oblnkl(outfile)         
         ! Assume upper and lower divertors geometries are symmetric.
         ptop_radial = rmajor - triu*rminor
         ptop_vertical = kap*rminor
         call ovarrf(outfile, 'Plasma top position, radial (m)', '(ptop_radial)', ptop_radial, 'OP ')
         call ovarrf(outfile, 'Plasma top position, vertical (m)', '(ptop_vertical)', ptop_vertical, 'OP ')
         call ovarrf(outfile, 'Plasma geometric centre, radial (m)', '(rmajor.)', rmajor, 'OP ')
         call ovarrf(outfile, 'Plasma geometric centre, vertical (m)', '(0.0)', 0.0d0, 'OP ')
         call ovarrf(outfile, 'Plasma triangularity', '(tril)', tril, 'OP ')
         call ovarrf(outfile, 'Plasma elongation', '(kappa.)', kap, 'OP ')
         call ovarrf(outfile, 'TF coil vertical offset (m)', '(tfoffset)', tfoffset, 'OP ')
         call ovarrf(outfile, 'Plasma upper X-pt, radial (m)', '(rxpt)', rxpt, 'OP ')
         call ovarrf(outfile, 'Plasma upper X-pt, vertical (m)', '(-zxpt)', -zxpt, 'OP ')
         call ovarrf(outfile, 'Plasma outer arc radius of curvature (m)', '(rco)', rco, 'OP ')
         call ovarrf(outfile, 'Plasma inner arc radius of curvature (m)', '(rci)', rci, 'OP ')
         call ovarrf(outfile, 'Plasma lower X-pt, radial (m)', '(rxpt)', rxpt, 'OP ')
         call ovarrf(outfile, 'Plasma lower X-pt, vertical (m)', '(zxpt)', zxpt, 'OP ')
         call ovarrf(outfile, 'Poloidal plane angle between vertical and inner leg (rad)', '(thetai)', thetai, 'OP ')
         call ovarrf(outfile, 'Poloidal plane angle between vertical and outer leg (rad)', '(thetao)', thetao, 'OP ')
         call ovarrf(outfile, 'Poloidal plane angle between inner leg and plate (rad)', '(betai)', betai)
         call ovarrf(outfile, 'Poloidal plane angle between outer leg and plate (rad)', '(betao)', betao)
         call ovarrf(outfile, 'Inner divertor leg poloidal length (m)', '(plsepi)', plsepi)
         call ovarrf(outfile, 'Outer divertor leg poloidal length (m)', '(plsepo)', plsepo)
         call ovarrf(outfile, 'Inner divertor plate length (m)', '(plleni)', plleni)
         call ovarrf(outfile, 'Outer divertor plate length (m)', '(plleno)', plleno) 
         call ovarrf(outfile, 'Upper inner strike point, radial (m)', '(rspi)', rspi, 'OP ')
         call ovarrf(outfile, 'Upper inner strike point, vertical (m)', '(-zspi)', -zspi, 'OP ')
         call ovarrf(outfile, 'Upper inner plate top, radial (m)', '(rplti)', rplti, 'OP ')
         call ovarrf(outfile, 'Upper inner plate top, vertical (m)', '(-zplti)', -zplti, 'OP ')
         call ovarrf(outfile, 'Upper inner plate bottom, radial (m)', '(rplbi)', rplbi, 'OP ')
         call ovarrf(outfile, 'Upper inner plate bottom, vertical (m)', '(-zplbi)', -zplbi, 'OP ')
         call ovarrf(outfile, 'Upper outer strike point, radial (m)', '(rspo)', rspo, 'OP ')
         call ovarrf(outfile, 'Upper outer strike point, vertical (m)', '(-zspo)', -zspo, 'OP ')
         call ovarrf(outfile, 'Upper outer plate top, radial (m)', '(rplto)', rplto, 'OP ')
         call ovarrf(outfile, 'Upper outer plate top, vertical (m)', '(-zplto)', -zplto, 'OP ')
         call ovarrf(outfile, 'Upper outer plate bottom, radial (m)', '(rplbo)', rplbo, 'OP ')
         call ovarrf(outfile, 'Upper outer plate bottom, vertical (m)', '(-zplbo)', -zplbo, 'OP ')
         call ovarrf(outfile, 'Lower inner strike point, radial (m)', '(rspi)', rspi, 'OP ')
         call ovarrf(outfile, 'Lower inner strike point, vertical (m)', '(zspi)', zspi, 'OP ')
         call ovarrf(outfile, 'Lower inner plate top, radial (m)', '(rplti)', rplti, 'OP ')
         call ovarrf(outfile, 'Lower inner plate top, vertical (m)', '(zplti)', zplti, 'OP ')
         call ovarrf(outfile, 'Lower inner plate bottom, radial (m)', '(rplbi)', rplbi, 'OP ')
         call ovarrf(outfile, 'Lower inner plate bottom, vertical (m)', '(zplbi)', zplbi, 'OP ')
         call ovarrf(outfile, 'Lower outer strike point, radial (m)', '(rspo)', rspo, 'OP ')
         call ovarrf(outfile, 'Lower outer strike point, vertical (m)', '(zspo)', zspo, 'OP ')
         call ovarrf(outfile, 'Lower outer plate top, radial (m)', '(rplto)', rplto, 'OP ')
         call ovarrf(outfile, 'Lower outer plate top, vertical (m)', '(zplto)', zplto, 'OP ')
         call ovarrf(outfile, 'Lower outer plate bottom, radial (m)', '(rplbo)', rplbo, 'OP ')
         call ovarrf(outfile, 'Lower outer plate bottom, vertical (m)', '(zplbo)', zplbo, 'OP ')
         call ovarrf(outfile, 'Calculated maximum divertor height (m)', '(divht)', divht, 'OP ') 
      else
         call oheadr(outfile, 'Divertor build and plasma position')
         call ocmmnt(outfile, 'ERROR: null value not supported, check i_single_null value.')
      end if
   
    
    end if

  end subroutine divgeom

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! subroutine rippl(ripmax,rmajor,rminor,r_tf_outboard_mid,n_tf,ripple,r_tf_outboard_midl)

  !   !! TF ripple calculation
  !   !! P J Knight, CCFE, Culham Science Centre
  !   !! ripmax : input real : max ripple at plasma edge (peak to average) (%)
  !   !! rmajor : input real : plasma major radius (m)
  !   !! rminor : input real : plasma minor radius (m)
  !   !! rtot   : input real : default radius to the outboard TF coil leg (m)
  !   !! tfno   : input real(!) : number of TF coils
  !   !! ripple : output real : ripple at plasma edge (%)
  !   !! rtotl  : output real : required minimum radius to the centre
  !   !!                        of the outboard TF coil leg (m)
  !   !! Subroutine to calculate TFC ripple and outboard TFC leg radius.
  !   !! Input the max. ripple and default outboard leg location and the
  !   !! routine checks to see if the ripple is OK. If not it moves
  !   !! the outboard leg appropriately.
  !   !
  !   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !   implicit none

  !   !  Arguments

  !   real(dp), intent(in) :: ripmax,rmajor,rminor,r_tf_outboard_mid,n_tf
  !   real(dp), intent(out) :: ripple,r_tf_outboard_midl

  !   !  Local variables

  !   real(dp) :: prip,rotrp,pripc,coeff

  !   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !   coeff = 1.03333D0 &
  !        + 0.210480D0 * n_tf &
  !        - 4.45253D-2 * n_tf**2 &
  !        + 3.50210D-3 * n_tf**3 &
  !        - 1.28945D-4 * n_tf**4 &
  !        + 1.84776D-6 * n_tf**5

  !   prip = 0.01D0 * ripmax/coeff
  !   rotrp = 1.023D0*(rmajor+rminor)/prip**(1.0D0/n_tf)

  !   if (rotrp > r_tf_outboard_mid) then
  !      r_tf_outboard_midl = rotrp
  !      pripc = prip * 100.0D0
  !      ripple = pripc * coeff
  !   else
  !      r_tf_outboard_midl = r_tf_outboard_mid
  !      prip = (1.023D0*(rmajor+rminor)/r_tf_outboard_mid)**(n_tf)
  !      pripc = prip*100.0D0
  !      ripple = pripc * coeff
  !   end if

  ! end subroutine rippl

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ripple_amplitude(ripple,ripmax,r_tf_outboard_mid,r_tf_outboard_midmin,flag)

    !! TF ripple calculation
    !! author: P J Knight, CCFE, Culham Science Centre
    !! ripmax : input real  : maximum allowed ripple at plasma edge (%)
    !! ripple : output real : actual ripple at plasma edge (%)
    !! rtot   : input real  : radius to the centre of the outboard
    !! TF coil leg (m)
    !! rtotmin : output real : radius to the centre of the outboard
    !! TF coil leg which would produce
    !! a ripple of amplitude ripmax (m)
    !! flag : output integer : on exit, =1 if the fitted
    !! range of applicability is exceeded
    !! This routine calculates the toroidal field ripple amplitude
    !! at the midplane outboard plasma edge. The fitted coefficients
    !! were produced from MATLAB runs by M. Kovari using the CCFE
    !! MAGINT code to model the coils and fields.
    !! <P>The minimum radius of the centre of the TF coil legs
    !! to produce the maximum allowed ripple is also calculated.
    !! M. Kovari, Toroidal Field Coils - Maximum Field and Ripple -
    !! Parametric Calculation, July 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use physics_variables, only: rminor, rmajor
		use tfcoil_variables, only: tinstf, wwp1, n_tf, tftort, casths
    implicit none

    !  Arguments

    integer, intent(out) :: flag
    real(dp), intent(in) :: ripmax,r_tf_outboard_mid
    real(dp), intent(out) :: ripple,r_tf_outboard_midmin

    !  Local variables

    real(dp) :: w, x, c1, c2, n

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    n = real(n_tf, kind(1.0D0))

    !  TF coil winding pack width

    if (wwp1 == 0.0D0) then  !  not yet calculated
       w = tftort - 2.0D0*(casths + tinstf)  !  rough estimate of wwp1
       x = w*n/rmajor
    else
       x = wwp1*n/rmajor
    end if

    c1 = 0.875D0 - 0.0557D0*x
    c2 = 1.617D0 + 0.0832D0*x

    !  Calculated ripple for coil at r_tf_outboard_mid (%)

    ripple = 100.0D0 * c1*( (rmajor+rminor)/r_tf_outboard_mid )**(n-c2)

    !  Calculated r_tf_outboard_mid to produce a ripple of amplitude ripmax

    r_tf_outboard_midmin = (rmajor+rminor) / &
         ( (0.01D0*ripmax/c1)**(1.0D0/(n-c2)) )

    !  Notify via flag if a range of applicability is violated

    flag = 0
    if ((x < 0.737D0).or.(x > 2.95D0)) flag = 1
    if ((n_tf < 16).or.(n_tf > 20)) flag = 2
    if ( ((rmajor+rminor)/r_tf_outboard_mid < 0.7D0).or. &
         ((rmajor+rminor)/r_tf_outboard_mid > 0.8D0) ) flag = 3

  end subroutine ripple_amplitude

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine portsz

    !! Port size calculation
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: M D Kovari, CCFE, Culham Science Centre
    !! None
    !! This subroutine finds the maximum possible tangency radius
    !! for adequate beam access.
    !! <P>The outputs from the routine are
    !! <UL> <P><LI>rtanbeam : Beam tangency radius (m)
    !! <P><LI>rtanmax : Maximum possible tangency radius (m) </UL>
    !! A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use build_variables, only: r_tf_outboard_mid, tfthko
		use constants, only: twopi
    use current_drive_variables, only: rtanbeam, rtanmax, nbshield, beamwd, &
      frbeam
		use error_handling, only: fdiags, report_error
		use physics_variables, only: rmajor
		use tfcoil_variables, only: tftort, n_tf
    implicit none

    !  Arguments

    !  Local variables

    real(dp) :: a,b,c,d,e,f,g,h
    real(dp) :: alpha,eps,theta,phi,omega

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Beam tangency radius (m)

    rtanbeam = frbeam * rmajor

    !  Toroidal angle between adjacent TF coils

    omega = twopi/n_tf

    !  Half-width of outboard TF coil in toroidal direction (m)

    a = 0.5D0*tftort  !  (previously used inboard leg width)

    !  Radial thickness of outboard TF coil leg (m)

    b = tfthko

    !  Width of beam duct, including shielding on both sides (m)

    c = beamwd + 2.0D0*nbshield

    !  Major radius of inner edge of outboard TF coil (m)

    d = r_tf_outboard_mid - 0.5D0*b

    !  Refer to figure in User Guide for remaining geometric calculations

    e = sqrt( a*a + (d+b)*(d+b) )
    f = sqrt( a*a + d*d )

    theta = omega - atan(a/d)
    phi = theta - asin(a/e)

    g = sqrt( e*e + f*f - 2.0D0*e*f*cos(phi) )  !  cosine rule

    if (g > c) then

       h = sqrt( g*g - c*c )

       alpha = atan(h/c)
       eps = asin(e*sin(phi)/g) - alpha  !  from sine rule

       !  Maximum tangency radius for centreline of beam (m)

       rtanmax = f*cos(eps) - 0.5D0*c

    else  !  coil separation is too narrow for beam...

       fdiags(1) = g ; fdiags(2) = c
       call report_error(63)

       rtanmax = 0.0D0

    end if

  end subroutine portsz

end module build_module
