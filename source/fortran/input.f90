 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  Uncomment #define line below to perform unit testing
!  Compile using pre-processor, e.g. ifort -cpp input.f90
!#define unit_test

module process_input

  !! Module containing the routines that perform the actual reading
  !! and parsing of the input file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module provides a set of routines to read in data from the
  !! main PROCESS input file (IN.DAT). The format of the file is
  !! similar to the F90 NAMELIST structure, but with a few
  !! additional features:
  !! <OL>
  !! <P><LI>Comments can be read in that are copied to the standard
  !! output channel - these are lines with five (or more)
  !! consecutive '*' characters at the start.
  !! <P><LI>Other lines within the file can contain simple comments
  !! for the user - these are not copied to the standard output
  !! channel. They start with one to four '*' characters.
  !! </OL>
  !! <P>Character strings, integers and double precision values can
  !! be read in.
  !! <P>The following rules must be obeyed when writing an input
  !! file:
  !! <UL>
  !! <P><LI>Each variable must be on a separate line.
  !! <P><LI>Leading spaces are ignored.
  !! <P><LI>Variable names can be upper case, lower case, or a
  !! mixture of both.
  !! <P><LI>Spaces may not appear within a variable name or data
  !! value.
  !! <P><LI>Other spaces within a line, and trailing spaces, are
  !! ignored.
  !! <P><LI>Commas are not necessary between variables.
  !! <P><LI>Data can extend over more than one line.
  !! <P><LI>One-dimensional arrays can be explicitly subscripted, or
  !! unscripted, in which case the following element order is
  !! assumed: A(1), A(2), A(3), ...
  !! <P><LI>At present, multiple dimension arrays can only be
  !! handled without reference to explicit subscripts, in which case
  !! the following element order is assumed: B(1,1), B(2,1), B(3,1),
  !! etc. The use of the input file to specify multiple dimension
  !! array elements is prone to error.
  !! <P><LI>Unscripted array elements must be separated by commas.
  !! <P><LI>Blank lines are allowed anywhere in the input file.
  !! <P><LI>Lines starting with a * are assumed to be comments.
  !! <P><LI>Comment lines starting with five or more asterisks
  !! (i.e. *****) are reproduced verbatim in the output file. These
  !! should be used copiously to give a great deal of information
  !! about the run being performed, and should be updated before
  !! every single run of the code, as it is very easy to lose track
  !! of what is being attempted.
  !! </UL>
  !! A User's Guide to the PROCESS Systems Code, P. J. Knight,
  !! AEA Fusion Report AEA FUS 251, 1993
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  private
  public :: input, check_range_int, check_range_real, lower_case
  integer, public, parameter :: nin = 10

#ifdef unit_test
  public :: parse_input_file
#endif
!  public :: upper_case

  integer, parameter :: maxlen = 300  !  maximum line length
  character(len=maxlen) :: line  !  current line of text from input file
  integer :: linelen, lineno  !  current line length, line number
  integer :: iptr             !  current position on line
  integer :: infile, outfile, report_changes, icode
  logical :: subscript_present
  logical :: error = .False.
  character(len=78) :: error_message

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine input

    !! Routine that calls the main input file parsing routines
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine provides the interface between the input file
    !! reading routines and the rest of PROCESS.
    !! A User's Guide to the PROCESS Systems Code, P. J. Knight,
    !! AEA Fusion Report AEA FUS 251, 1993
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: nout
    use numerics, only: ipeqns, icc, active_constraints
    implicit none

    !  Arguments

    !  Local variables

    integer :: show_changes = 0
    integer :: i
    !     j
    logical :: constraints_exist=.false.
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call parse_input_file(nin,nout,show_changes)

    ! Set all the values of the active_constraints array
    do i = 1, ipeqns
        if (icc(i) /= 0) then
            active_constraints(icc(i)) = .true.
            constraints_exist = .true.
        end if
    end do

  end subroutine input

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_input_file(in_file,out_file,show_changes)

    !! Routine that parses the contents of the input file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! author: F Warmer, IPP Greifswald
    !! in_file  : input integer : Fortran input unit identifier
    !! out_file : input integer : Fortran output unit identifier
    !! show_changes : input integer : switch to turn on (1) or off (0)
    !! reporting of changed values
    !! This routine reads the data from the PROCESS input file (IN.DAT),
    !! dealing with comments or blank lines correctly, and sets the
    !! value of any variables found in the file. Any changes
    !! from the default values may be reported if required.
    !! <P>Each possible variable in this block is dealt with
    !! individually. (To add additional input variables, simply copy
    !! and edit one of the similar existing examples.)
    !! The routine also does the extremely useful function of checking
    !! that the given value for a variable lies within a sensible
    !! predefined range, and stops the program if apparently
    !! nonsensical values are attempted.
    !! A User's Guide to the PROCESS Systems Code, P. J. Knight,
    !! AEA Fusion Report AEA FUS 251, 1993
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: dcopper, dalu
    use global_variables, only: run_tests, verbose, maxcal, runtitle
    use build_variables, only: fmsfw, blbmoth, blbuith, fmsbc, shldoth, &
      fmsdwi, shldtth, shldlth, vgap2, plleni, fwoth, vvblgap, fmsbl, &
      thshield, iprecomp, blbpith, aplasmin, blbuoth, tfcth, fmsdwe, &
      iohcl, tftsgap, clhsf, bore, plleno, scrapli, gapomin, ddwex, &
      rinboard, fmstf, blnkoth, fseppc, plsepo, ddwi, fmssh, blnkith, &
      ohcth, plsepi, fmsoh, blbmith, gapoh, fcspc, scraplo, vgaptop, &
      blbpoth, gapds, fwith, vgap, shldith, sigallpc, tfootfi, f_avspace,&
      r_cp_top
    use buildings_variables, only: hcwt, conv, wgt, trcl, rxcl, rbwt, mbvfac, &
      esbldgm3, rbvfac, fndt, row, wgt2, pibv, clh1, stcl, clh2, pfbldgm3, &
      shmf, tfcbv, hccl, rbrt, triv, shov, admv, wsvfac
    use constraint_variables, only: flhthresh, fpeakb, fpsep, fdivcol, ftcycl, &
      betpmx, fpsepbqar, ftmargtf, fradwall, fptfnuc, fnesep, fportsz, tbrmin, &
      maxradwallload, pseprmax, fdene, fniterpump, fpinj, pnetelin, powfmax, &
      fgamcd, ftbr, mvalim, taulimit, walalw, fmva, fradpwr, nflutfmax, fipir, &
      fauxmn, fiooic, fcwr, fjohc0, frminor, psepbqarmax, ftpeak, bigqmin, &
      fstrcond, fptemp, ftmargoh, fvs, fbetatry, vvhealw, fpnetel, ftburn, &
      ffuspow, fpsepr, ptfnucmax, fvdump, pdivtlim, ftaulimit, nbshinefmax, &
      fcqt, fzeffmax, fstrcase, fhldiv, foh_stress, fwalld, gammax, fjprot, &
      ftohs, tcycmn, auxmin, zeffmax, peakfactrad, fdtmp, fpoloidalpower, &
      fnbshinef, freinke, fvvhe, fqval, fq, ftaucq, fbetap, fbeta, fjohc, &
      fflutf, bmxlim, tbrnmn, fbetatry_lower
    use cost_variables, only: ucich, uctfsw, dintrt, ucblbe, uubop, dtlife, &
      cost_factor_vv, cfind, uccry, fcap0cp, uccase, uuves, cconshtf, conf_mag, &
      ucbllipb, ucfuel, uumag, ucpfbs, ireactor, uucd, div_umain_time, div_nu, &
      maintenance_gen, uctfps, uufw, tbktrepl, cost_factor_fwbs, decomf, &
      cconshpf, uche3, ucpfdr1, ucech, uudiv, cost_model, adivflnc, &
      cost_factor_rh, cost_factor_bop, ifueltyp, fcontng, fwbs_nref, &
      cost_factor_buildings, favail, cconfix, ucblli2o, abktflnc, ucf1, ucfnc, &
      ucpfps, iavail, ucpfbk, cost_factor_tf_coils, costexp_pebbles, ucmisc, &
      cpstflnc, uccryo, costexp, fwbs_nu, ucpfic, ucblbreed, tcomrepl, uufuel, &
      ucdiv, uccpcl1, ratecdol, uctfbr, uccpclb, ucoam, div_prob_fail, ucnbi, &
      uccu, ucwst, cfactr, div_nref, amortization, ucwindtf, ucme, csi, cowner, &
      cost_factor_misc, fcr0, cturbb, lsa, fcap0, output_costs, &
      cost_factor_land, redun_vacp, ucrb, uctfbus, num_rh_systems, fkind, &
      fwbs_umain_time, uchrs, avail_min, uciac, step_ref, ucshld, tdivrepl, &
      ucblli, ucpfcb, tlife, ipnet, fcdfuel, ucbus, ucpfb, uchts, &
      maintenance_fwbs, fwbs_prob_fail, uclh, ucblss, ucblvd, ucsc, ucturb, &
      ucpens, cland, ucwindpf 
    use current_drive_variables, only: pinjfixmw, etaech, pinjalw, etanbi, &
      ftritbm, gamma_ecrh, pheat, rho_ecrh, beamwd, enbeam, pheatfix, bscfmax, &
      forbitloss, nbshield, tbeamin, feffcd, iefrf, iefrffix, irfcd, cboot, &
      etalh, frbeam 
    use div_kal_vars, only: kallenbach_test_option, &
      relerr_sol, kallenbach_scan_switch, lcon_factor, kallenbach_scan_num, &
      kallenbach_scan_end, kallenbach_scan_start, target_spread, &
      fractionwidesol, impurity_enrichment, mach0, kallenbach_scan_var, &
      abserr_sol, qtargettotal, lambda_q_omp, ttarget, kallenbach_tests, &
      kallenbach_switch, netau_sol, neratio, targetangle 
    use divertor_variables, only: fdfs, anginc, divdens, divclfr, c4div, &
      c5div, ksic, fififi, divplt, delld, c2div, betao, divdum, tdiv, c6div, &
      omegan, prn1, fgamp, frrp, xpertin, c1div, betai, bpsout, xparain, fdiva, &
      zeffdiv, hldivlim, rlenmax, divfix, c3div 
    use fwbs_variables, only: fblhebpo, vfblkt, fdiv, fvolso, fwcoolant, &
      pitch, iblanket, blktmodel, afwi, fblli2o, nphcdin, breeder_multiplier, &
      fw_armour_thickness, roughness, fwclfr, breedmat, fblli, fblvd, &
      iblanket_thickness, vfcblkt, breeder_f, fbllipb, fhcd, vfshld, fblhebmi, &
      denw, f_neut_shield, fw_th_conductivity, nblktmodti, fw_wall, afwo, &
      fvolsi, etahtp, nblktmodpo, fwpressure, emult, fwoutlet, nblktmodpi, &
      fblhebpi, fblss, inlet_temp, outlet_temp, fblbreed, qnuc, blpressure, &
      declblkt, fblhebmo, blkttype, afw, inuclear, declshld, hcdportsize, &
      npdiv, peaking_factor, primary_pumping, rpf2dewar, secondary_cycle, &
      denstl, declfw, nphcdout, iblnkith, vfpblkt, fwinlet, wallpf, fblbe, &
      fhole, fwbsshape, coolp, tfwmatmax, irefprop, fw_channel_length, &
      li6enrich, etaiso, nblktmodto, fvoldw 
    use heat_transport_variables, only: htpmw_fw, baseel, fmgdmw, htpmw_div, &
      pwpm2, etath, vachtmw, iprimshld, fpumpdiv, pinjmax, htpmw_blkt, etatf, &
      htpmw_min, fpumpblkt, ipowerflow, htpmw_shld, fpumpshld, trithtmw, &
      iprimnloss, fpumpfw 
    use ife_variables, only: bldzu, etali, sombdr, gainve, cdriv0, v1dzl, &
      bldrc, fauxbop, pfusife, dcdrv0, fwdr, pdrive, mcdriv, ucconc, shdr, &
      v3dzu, bldzl, rrin, maxmat, shmatf, fwmatf, drveff, flirad, shdzu, v2dzu, &
      pifecr, ifedrv, v2dr, chmatf, v1dr, v1matf, dcdrv1, chdzu, dcdrv2, &
      ifetyp, fwdzl, htpmw_ife, uccarb, v3matf, fbreed, edrive, ptargf, cdriv2, &
      fburn, fwdzu, etave, v3dr, uctarg, shdzl, ucflib, v3dzl, v1dzu, v2dzl, &
      chdzl, chrad, cdriv1, tgain, somtdr, v2matf, rrmax, bldr, frrmax , blmatf
    use impurity_radiation_module, only: coreradius, nimp, impvar, fimpvar, &
      coreradiationfraction, impdir, fimp
    use numerics, only: factor, boundl, minmax, neqns, nvar, epsfcn, ixc, &
      epsvmc, ftol, ipnvars, ioptimz, nineqns, ipeqns, boundu, icc, ipnfoms, name_xc
    use pfcoil_variables, only: rjconpf, zref, fcuohsu, oh_steel_frac, vf, &
      coheof, sigpfcalw, alstroh, ipfres, fcupfsu, fvssu, etapsu, i_cs_stress, &
      fbmaxcs, ngc, rpf2, fcohbop, ohhghf, vfohc, isumatoh, ngrpmx, ngc2, rpf1, &
      ngrp, isumatpf, nfxfh, alfapf, routr, sigpfcf, pfclres, bmaxcs_lim, &
      ncls, nfixmx, cptdin, ipfloc 
    use physics_variables, only: ipedestal, taumax, i_single_null, fvsbrnni, &
      rhopedt, cvol, fdeut, ffwal, eped_sf, iculbl, itartpf, ilhthresh, &
      fpdivlim, epbetmax, isc, kappa95, aspect, cwrmax, nesep, csawth, dene, &
      ftar, plasma_res_factor, ssync, rnbeam, beta, neped, hfact, dnbeta, &
      fgwsep, rhopedn, tratio, q0, ishape, fne0, ignite, igeom, ftrit, &
      ifalphap, tauee_in, alphaj, alphat, icurr, q, ti, tesep, rli, triang, &
      itart, ralpne, iprofile, triang95, rad_fraction_sol, betbm0, protium, &
      teped, fhe3, iwalld, gamma, falpha, fgwped, gtscale, tbeta, ibss, &
      iradloss, te, alphan, rmajor, kappa, ifispact, iinvqd, fkzohm, beamfus0, &
      tauratio, idensl, ieped, bt, iscrp, ipnlaws, betalim, betalim_lower, &
      idia, ips
    use pf_power_variables, only: iscenr, maxpoloidalpower 
    use plasmod_variables, only: plasmod_x_control, plasmod_i_modeltype, &
      plasmod_nx, plasmod_chisaw, plasmod_contrpovr, plasmod_dtmax, &
      plasmod_eccdeff, plasmod_isawt, plasmod_dtinc, plasmod_eopt, &
      plasmod_qdivt, plasmod_dx_cd, plasmod_maxa, plasmod_qnbi_psepfac, &
      plasmod_dx_heat, plasmod_i_equiltype, plasmod_psepplh_sup, &
      plasmod_imptype, plasmod_sawpertau, plasmod_dtmaxmax, &
      plasmod_gamcdothers, plasmod_i_impmodel, plasmod_dtmaxmin, plasmod_test, &
      plasmod_iprocess, plasmod_ainc, plasmod_fcdp, plasmod_x_fus, &
      plasmod_dx_fus, plasmod_v_loop, plasmod_maxpauxor, plasmod_dtmin, &
      plasmod_nchannels, plasmod_spellet, plasmod_pedscal, plasmod_capa, &
      plasmod_dgy, plasmod_dt, plasmod_tol, plasmod_fpellet, plasmod_contrpovs, &
      plasmod_x_heat, plasmod_fradc, plasmod_tolmin, plasmod_pech, &
      plasmod_globtau, plasmod_pfus, plasmod_nbi_energy, plasmod_nxt, &
      plasmod_x_cd, plasmod_chisawpos, plasmod_cxe_psepfac, plasmod_dx_control, &
      plasmod_car_qdivt 
    use pulse_variables, only: lpulse, dtstor, itcycl, istore, bctmp 
    use scan_module, only: isweep_2, nsweep, isweep, scan_dim, nsweep_2, &
      sweep_2, sweep, ipnscns, ipnscnv 
    use stellarator_variables, only: f_asym, isthtr, n_res, iotabar, fdivwet, &
      f_w, bmn, shear, m_res, f_rad, flpitch, istell
    use tfcoil_variables, only: fcoolcp, tfinsgap, vftf, &
      quench_detection_ef, fhts, dr_tf_wp, rcool, rhotfleg, thkcas, &
      casthi, n_pancake, bcritsc, i_tf_sup, strncon_pf, thwcndut, farc4tf, &
      thicndut, tftmp, oacdcp, tmax_croco, ptempalw, tmargmin_tf, tmpcry, &
      alstrtf, dztop, dcond, strncon_cs, etapump, drtop, vcool, dcondins, &
      i_tf_tresca, dhecoil, tmaxpro, strncon_tf, n_tf, tcpav, fcutfsu, jbus, &
      casthi_fraction, tmargmin_cs, sigvvall, vdalw, dcase, t_turn,&
      cpttf_max, tdmptf, casths, i_tf_turns_integer, quench_model, &
      tcritsc, layer_ins, tinstf, n_layer, tcoolin, ripmax, frhocp, &
      cpttf, tmargmin, casths_fraction, eff_tf_cryo, eyoung_ins, &
      eyoung_steel, eyoung_res_tf_buck, eyoung_winding, f_vforce_inboard, &
      fcoolleg, frholeg, ftoroidalgap, i_tf_sc_mat, i_tf_shape, i_tf_bucking, &
      n_tf_graded_layers, n_tf_joints, n_tf_joints_contact, poisson_al, &
      poisson_copper, poisson_steel, rho_tf_joints, rhotfbus, th_joint_contact,&
      i_tf_plane_stress, eyoung_al, i_tf_wp_geom, i_tf_case_geom, &
      i_tf_turns_integer, n_rad_per_layer, b_crit_upper_nbti, t_crit_nbti

    use times_variables, only: tohs, pulsetimings, tqnch, theat, tramp, tburn, &
      tdwell, tohsin 
    use vacuum_variables, only: dwell_pump, pbase, tn, pumpspeedfactor, &
      initialpressure, outgasfactor, prdiv, pumpspeedmax, rat, outgasindex, &
      pumpareafraction, ntype, vacuum_model, pumptp 
    use rebco_variables, only: hastelloy_thickness, f_coppera_m2, &
      rebco_thickness, copper_rrr, coppera_m2_max, croco_thick, copper_thick 
    use reinke_variables, only: reinke_mode, fzactual, impvardiv, lhat 
    implicit none

    !  Arguments

    integer, intent(in) :: in_file, out_file, show_changes

    !  Local variables

    integer :: iost
    integer :: isub1,isub2,varlen
    integer :: no_constraints=0
    integer :: no_iteration=0
    integer :: foundAst

    character(len=32) :: varnam

    logical :: obsolete_var = .false.
    character(len=400) :: imp_dir 
    
    imp_dir = impdir()

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Initialise module-wide variables

    infile = in_file
    outfile = out_file
    report_changes = show_changes

    icode = 0
    lineno = 0

    !  Main loop

    loop_over_lines: do

       subscript_present = .FALSE.

       read(infile,'(A)',iostat=iost) line

       !  On error or end, return
       if (iost /= 0) exit loop_over_lines

       lineno = lineno + 1

       line = adjustl(line)  !  rotate any leading blanks to the end
       linelen = len_trim(line)


20     continue

       !  Ignore blank lines

       if (line == ' ') cycle

       !  Ignore comments, unless they start with '*****',
       !  in which case print them.

       if (line(1:5) == '*****') write(outfile,*) line(1:76)
       if (line(1:1) == '*') cycle
       if (line(1:1) == '$') cycle  !  in case block delimiters are still present

       iptr = 1

       !Ignore input comments denoted by asterisk, before assigning variables

       if (index(line,'*') > 0) then
          foundAst = index(line,'*') - 1
          linelen = min(linelen, foundAst)
          line = line(:linelen)
       end if

       !  This must be an assignment line, so get the variable name

       call get_variable_name(varnam,varlen,isub1,isub2)
       if (isub1 /= 0) subscript_present = .TRUE.
       if (varlen == 0) then
          write(*,*) 'Error in IN.DAT at line ', lineno
          write(*,*) line
          error = .True.
       end if

       !  Read the associated data

       variable: select case (varnam(1:varlen))

          !  General settings

       case ('runtitle')
          call parse_string_variable('runtitle', runtitle, &
               'title of run')
       case ('verbose')
          call parse_int_variable('verbose', verbose, 0, 1, &
               'Switch for diagnostic output')
       case ('run_tests')
          call parse_int_variable('run_tests', run_tests, 0, 1, &
               'Switch for running built-in tests')

          !  Numerical solver settings

       case ('boundl')
          call parse_real_array('boundl', boundl, isub1, ipnvars, &
               'Iteration variable lower bound', icode)
       case ('boundu')
          call parse_real_array('boundu', boundu, isub1, ipnvars, &
               'Iteration variable upper bound', icode)
       case ('epsfcn')
          call parse_real_variable('epsfcn', epsfcn, 0.0D0, 1.0D0, &
               'HYBRD/VMCON derivative step length')
       case ('epsvmc')
          call parse_real_variable('epsvmc', epsvmc, 0.0D0, 1.0D0, &
               'VMCON error tolerance')
       case ('factor')
          call parse_real_variable('factor', factor, 0.0D0, 10.0D0, &
               'HYBRD initial step size')
       case ('ftol')
          call parse_real_variable('ftol', ftol, 0.0D0, 1.0D0, &
               'HYBRD tolerance')

       ! New optional argument startindex used MDK 3/3/17
       ! Allows simplified IN.DAT format for icc and ixc.
       case ('icc')
          no_constraints = no_constraints + 1
          call parse_int_array('icc', icc, isub1, ipeqns, &
               'Constraint equation', icode,no_constraints)
          no_constraints = isub1
      case ('ixc')
          no_iteration = no_iteration + 1
          call parse_int_array('ixc', ixc, isub1, ipnvars, &
                   'Iteration variable', icode,no_iteration)
          no_iteration = isub1

       case ('ioptimz')
          call parse_int_variable('ioptimz', ioptimz, -2, 1, &
               'Switch for solver method')
       case ('maxcal')
          call parse_int_variable('maxcal', maxcal, 0, 10000, &
               'Max no of VMCON iterations')
       case ('minmax')
          call parse_int_variable('minmax', minmax, -ipnfoms, ipnfoms, 'Switch for figure of merit')
       case ('neqns')
           write(*,*)'The total number of constraints is counted automatically and does not need to be stated in IN.DAT.'
           call parse_int_variable('neqns', neqns, 1, ipeqns, 'No of equality constraints')
       case ('nineqns')
          call parse_int_variable('nineqns', nineqns, 1, ipeqns, 'No of inequality constraints')
       case ('nvar')
          write(*,*)'The number of iteration variables is counted automatically and does not need to be stated in IN.DAT.'
       !  call parse_int_variable('nvar', nvar, 1, ipnvars, 'No of independent variables')

          !  Physics settings

       case ('alphaj')
          call parse_real_variable('alphaj', alphaj, 0.0D0, 10.0D0, &
               'Current density profile factor')
       case ('alphan')
          call parse_real_variable('alphan', alphan, 0.0D0, 10.0D0, &
               'Density profile factor')
       case ('alphat')
          call parse_real_variable('alphat', alphat, 0.0D0, 10.0D0, &
               'Temperature profile factor')
       case ('aspect')
          call parse_real_variable('aspect', aspect, 1.001D0, 20.0D0, &
               'Aspect ratio')
       case ('beamfus0')
          call parse_real_variable('beamfus0', beamfus0, 0.01D0, 10.0D0, &
               'Beam-background fusion multiplier')
       case ('beta')
          call parse_real_variable('beta', beta, 0.0D0, 1.0D0, &
               'Plasma beta')
       case ('betalim')
          call parse_real_variable('betalim', betalim, 0.0D0, 1.0D0, &
              'Plasma beta upper limit')
       case ('betalim_lower')
          call parse_real_variable('betalim_lower', betalim_lower, 0.0D0, 1.0D0, &
                'Plasma beta lower limit')
       case ('betbm0')
          call parse_real_variable('betbm0', betbm0, 0.0D0, 10.0D0, &
               'Leading coeff. for NB beta fraction')
       case ('bt')
          call parse_real_variable('bt', bt, 0.0D0, 30.0D0, &
               'Toroidal field on axis (T)')
       case ('coreradius')
          call parse_real_variable('coreradius', coreradius, 0.0D0, 1.0D0, &
               'Normalised core radius')
       case ('coreradiationfraction')
          call parse_real_variable('coreradiationfraction', coreradiationfraction, 0.0D0, 1.0D0, &
               'Fraction of core radiation subtracted from P_L')
       case ('csawth')
          call parse_real_variable('csawth', csawth, 0.0D0, 10.0D0, &
               'Coefficient for sawteeth effects')
       case ('cvol')
          call parse_real_variable('cvol', cvol, 0.01D0, 10.0D0, &
               'Plasma volume multiplier')
       case ('cwrmax')
          call parse_real_variable('cwrmax', cwrmax, 1.0D0, 3.0D0, &
               'Max conducting shell to rminor radius')
       case ('dene')
          call parse_real_variable('dene', dene, 1.0D18, 1.0D22, &
               'Electron density (/m3)')
       case ('dnbeta')
          call parse_real_variable('dnbeta', dnbeta, 0.0D0, 20.0D0, &
               'beta coefficient')
       case ('epbetmax')
          call parse_real_variable('epbetmax', epbetmax, 0.01D0, 10.0D0, &
               'Max epsilon*beta value')
       case ('eped_sf')
          call parse_real_variable('eped_sf', eped_sf, 0.0001D0, 2.0D0, &
               'Scaling factor for EPED pedestal model')
       case ('falpha')
          call parse_real_variable('falpha', falpha, 0.0D0, 1.0D0, &
               'Fraction of alpha power deposited to plasma')
       case ('ftar')
          call parse_real_variable('ftar', ftar, 0.0D0, 1.0D0, &
               'Fraction of power to divertor with lower divertor in double null')
       case ('fdeut')
          call parse_real_variable('fdeut', fdeut, 0.0D0, 1.0D0, &
               'Deuterium fuel fraction')
       case ('ffwal')
          call parse_real_variable('ffwal', ffwal, 0.0D0, 10.0D0, &
               'Wall load fiddle factor')
       case ('fgwped')
          call parse_real_variable('fgwped', fgwped, -1.0D0, 5.0D0, &
               'Fraction of n_G at pedestal top')
       case ('fgwsep')
          call parse_real_variable('fgwsep', fgwsep, -1.0D0, 1.0D0, &
               'Fraction of n_G at separatrix')
       case ('fhe3')
          call parse_real_variable('fhe3', fhe3, 0.0D0, 1.0D0, &
               'Helium-3 fuel fraction')
       case ('fimp')
          call parse_real_array('fimp', fimp, isub1, nimp, &
               'Impurity density fraction', icode)
       case ('fimpvar')
          call parse_real_variable('fimpvar', fimpvar, 1.0D-6, 0.5D0, &
               'Impurity fraction to be varied')
       case ('fkzohm')
          call parse_real_variable('fkzohm', fkzohm, 0.5D0, 2.0D0, &
               'Zohm elongation scaling multiplier')
       case ('fnesep')
          call parse_real_variable('fnesep', fnesep, 0.1D0, 2.0D1, &
               'Eich critical separatrix density')
       case ('fradmin')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'FRADMIN is now obsolete -'
          write(outfile,*) 'please remove it from the input file.'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('ftaulimit')
          call parse_real_variable('ftaulimit', ftaulimit, 0.001D0, 1.0D0, &
               'f-value for lower limit on taup/taueff the ratio of alpha particle to energy confinement times')

       case ('ftr')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'FTR is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '(use FTRIT instead).'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('ftrit')
          call parse_real_variable('ftrit', ftrit, 0.0D0, 1.0D0, &
               'Tritium fuel fraction')
       case ('fvsbrnni')
          call parse_real_variable('fvsbrnni', fvsbrnni, 0.0D0, 1.0D0, &
               'Non-inductive volt-sec burn fraction')
       case ('gamma')
          call parse_real_variable('gamma', gamma, 0.1D0, 1.0D0, &
               'Ejima coefficient for resistive V-s formula')
       case ('gtscale')
          call parse_int_variable('gtscale', gtscale, 0, 1, &
               'Flag to scale beta coefficient with R/a')
       case ('hfact')
          call parse_real_variable('hfact', hfact, 0.01D0, 10.0D0, &
               'Energy confinement time H factor')
       case ('taumax')
          call parse_real_variable('taumax', taumax, 0.1D0, 100.0D0, &
               'Maximum allowed energy confinement time (s)')
       case ('ibss')
          call parse_int_variable('ibss', ibss, 1, 4, &
               'Switch for bootstrap scaling')
       case ('iculbl')
          call parse_int_variable('iculbl', iculbl, 0, 2, &
               'Switch for beta limit scaling')
       case ('iculdl')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'ICULDL is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '(use IDENSL=3 for equivalent model to ICULDL=0).'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('icurr')
          call parse_int_variable('icurr', icurr, 1, 9, &
               'Switch for plasma current scaling')
       case ('idensl')
          call parse_int_variable('idensl', idensl, 1, 7, &
               'Switch for enforced density limit')
       case ('idhe3')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'IDHE3 is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '(use fhe3 to adjust 3He fuel fraction).'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('idia')
          call parse_int_variable('idia', idia, 0, 2, &
                'Switch for diamagnetic scaling')
       case ('ifalphap')
          call parse_int_variable('ifalphap', ifalphap, 0, 1, &
               'Switch for fast alpha pressure fit')
       case ('ifispact')
          call parse_int_variable('ifispact', ifispact, 0, 0, &
               'Switch for neutronics calculations')
       case ('igeom')
          call parse_int_variable('igeom', igeom, 0, 1, &
               'Switch for plasma geometry calculation')
       case ('ignite')
          call parse_int_variable('ignite', ignite, 0, 1, &
               'Switch for ignited plasma assumption')
       case ('iinvqd')
          call parse_int_variable('iinvqd', iinvqd, 0, 1, &
               'Switch for inverse quadrature')
       case ('iiter')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'IITER is now obsolete -'
          write(outfile,*) 'please remove it from the input file.'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('ilhthresh')
          call parse_int_variable('ilhthresh', ilhthresh, 1, 21, &
               'Switch for L-H power threshold to enforce')
       case ('impdir')
          call parse_string_variable('impdir', imp_dir, &
               'Directory containing impurity radiation data files')

       case ('impvar')
          call parse_int_variable('impvar', impvar, 3, nimp, &
               'Index for impurity fraction iteration variable')
          write(outfile,*) 'impvar is now deprecated - use iteration variables 125-136 instead.'
       case ('ipedestal')
          call parse_int_variable('ipedestal', ipedestal, 0, 3, &
               'Switch for plasma profile type')
       case ('iprofile')
          call parse_int_variable('iprofile', iprofile, 0, 1, &
               'Switch for current profile consistency')
       case ('ips')
          call parse_int_variable('ips', ips, 0, 1, &
               'Switch for Pfirsch-Schlüter scaling')
       case ('iradloss')
          call parse_int_variable('iradloss', iradloss, 0, 2, &
               'Switch for radiation loss term inclusion in pwr balance')
       case ('ires')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'IRES is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('isc')
          call parse_int_variable('isc', isc, 1, ipnlaws, &
               'Switch for confinement scaling law')
       case ('ieped')
          call parse_int_variable('ieped', ieped, 0, 1, &
               'Switch for scaling pedestal-top temperature with plasma parameters')

       case ('iscrp')
          call parse_int_variable('iscrp', iscrp, 0, 1, &
               'Switch for scrapeoff width')
       case ('ishape')
          call parse_int_variable('ishape', ishape, 0, 4, &
               'Switch for plasma shape vs. aspect')
       case ('itart')
          call parse_int_variable('itart', itart, 0, 1, &
               'Switch for tight aspect ratio physics')
       case ('itartpf')
                call parse_int_variable('itartpf', itartpf, 0, 1, &
               'Switch for tight aspect ratio PF coils')
       case ('iwalld')
          call parse_int_variable('iwalld', iwalld, 1, 2, &
               'Switch for wall load calculation')
       case ('kappa')
          call parse_real_variable('kappa', kappa, 0.99D0, 5.0D0, &
               'Plasma separatrix elongation')
       case ('kappa95')
          call parse_real_variable('kappa95', kappa95, 0.99D0, 5.0D0, &
               'Plasma 95% elongation')
       case ('neped')
          call parse_real_variable('neped', neped, 0.0D0, 1.0D21, &
               'Electron density pedestal height (/m3)')
       case ('neratio')
          call parse_real_variable('neratio', neratio, 0.001D0, 1.0D0, &
               'ratio of mean SOL density at OMP to separatrix density at OMP')
       case ('nesep')
          call parse_real_variable('nesep', nesep, 0.0D0, 1.0D21, &
               'Electron density at separatrix (/m3)')
       case ('plasma_res_factor')
          call parse_real_variable('plasma_res_factor', plasma_res_factor, 0.0D0, 1.0D0, &
               'Plasma resistivity pre-factor')
       case ('q')
          call parse_real_variable('q', q, 1.00D0, 50.0D0, &
               'Edge safety factor')
       case ('q0')
          call parse_real_variable('q0', q0, 0.01D0, 20.0D0, &
               'Safety factor on axis')
       case ('tauratio')
          call parse_real_variable('tauratio', tauratio, 0.1D0, 100.0D0, &
               'Ratio of He and pellet particle confinement times')
       case ('rad_fraction_sol')
          call parse_real_variable('rad_fraction_sol', rad_fraction_sol, 0.0D0, 1.0D0, &
               'SoL radiation fraction')
       case ('ralpne')
          call parse_real_variable('ralpne', ralpne, 1.0D-12, 1.0D0, &
               'Thermal alpha density / electron density')
       case ('protium')
          call parse_real_variable('protium', protium, 0.0D0, 1.0D0, &
               'Protium density / electron density')

       case ('rhopedn')
          call parse_real_variable('rhopedn', rhopedn, 0.01D0, 1.0D0, &
               'Density pedestal r/a')
       case ('rhopedt')
          call parse_real_variable('rhopedt', rhopedt, 0.01D0, 1.0D0, &
               'Temperature pedestal r/a')
       case ('rli')
          call parse_real_variable('rli', rli, 0.0D0, 10.0D0, &
               'Normalised inductivity')
       case ('rmajor')
          call parse_real_variable('rmajor', rmajor, 0.1D0, 50.0D0, &
               'Plasma major radius (m)')
       case ('rnbeam')
          call parse_real_variable('rnbeam', rnbeam, 0.0D0, 1.0D0, &
               'Hot beam density / electron density')
       case ('i_single_null')
          call parse_int_variable('i_single_null', i_single_null, 0, 1, &
               'Switch for single/double null plasma')
       case ('ssync')
          call parse_real_variable('ssync', ssync, 0.0D0, 1.0D0, &
               'Synchrotron wall reflectivity factor')
       case ('tbeta')
          call parse_real_variable('tbeta', tbeta, 0.0D0, 4.0D0, &
               'Temperature profile index beta')
       case ('te')
          call parse_real_variable('te', te, 1.0D0, 200.0D0, &
               'Electron temperature (keV)')
       case ('tauee_in')
           call parse_real_variable('tauee_in', tauee_in, 0.0D0, 100.0D0, &
                    'Input electron energy confinement time (sec) (isc=48 only)')
       case ('taulimit')
          call parse_real_variable('taulimit', taulimit, 1.0D0, 100.0D0, &
               'Lower limit on taup/taueff the ratio of alpha particle to energy confinement times')

       case ('teped')
          call parse_real_variable('teped', teped, 0.0D0, 20.0D0, &
               'Electron temperature pedestal height (keV)')
       case ('tesep')
          call parse_real_variable('tesep', tesep, 0.0D0, 20.0D0, &
               'Electron temperature at separatrix (keV)')
       case ('ti')
          call parse_real_variable('ti', ti, 5.0D0, 50.0D0, &
               'Ion temperature (keV)')
       case ('tratio')
          call parse_real_variable('tratio', tratio, 0.0D0, 2.0D0, &
               'Ion / electron temperature ratio')
       case ('triang')
          call parse_real_variable('triang', triang, -1.0D0, 1.0D0, &
               'Plasma separatrix triangularity')
       case ('triang95')
          call parse_real_variable('triang95', triang95, 0.0D0, 1.0D0, &
               'Plasma 95% triangularity')

          !  Inequality settings

       case ('fniterpump')
          call parse_real_variable('fniterpump', fniterpump, 0.001D0, 10.0D0, &
               'f-value for constraint on number of vacuum pumps')

       case ('auxmin')
          call parse_real_variable('auxmin', auxmin, 0.01D0, 100.0D0, &
               'Minimum auxiliary power (MW)')
       case ('betpmx')
          call parse_real_variable('betpmx', betpmx, 0.01D0, 2.0D0, &
               'Maximum poloidal beta')
       case ('bigqmin')
          call parse_real_variable('bigqmin', bigqmin, 0.01D0, 100.0D0, &
               'Minimum fusion gain Q')
       case ('bmxlim')
          call parse_real_variable('bmxlim', bmxlim, 0.1D0, 50.0D0, &
               'Maximum toroidal field (T)')
       case ('fauxmn')
          call parse_real_variable('fauxmn', fauxmn, 0.001D0, 10.0D0, &
               'F-value for minimum auxiliary power')
       case ('fbeta')
          call parse_real_variable('fbeta', fbeta, 0.001D0, 10.0D0, &
               'F-value for eps.betap beta limit')
       case ('fbetap')
          call parse_real_variable('fbetap', fbetap, 0.001D0, 10.0D0, &
               'F-value for poloidal beta limit')
       case ('fbetatry')
          call parse_real_variable('fbetatry', fbetatry, 0.001D0, 10.0D0, &
               'F-value for beta limit')
       case ('fbetatry_lower')
          call parse_real_variable('fbetatry_lower', fbetatry_lower, 0.001D0, 10.0D0, &
                  'F-value for (lower) beta limit')
       case ('fcwr')
          call parse_real_variable('fcwr', fcwr, 0.001D0, 10.0D0, &
               'F-value for conducting wall radius')
       case ('fdene')
          call parse_real_variable('fdene', fdene, 0.001D0, 10.0D0, &
               'F-value for density limit')
       case ('fdivcol')
          call parse_real_variable('fdivcol', fdivcol, 0.001D0, 10.0D0, &
               'F-value for divertor collisionality')
       case ('fdtmp')
          call parse_real_variable('fdtmp', fdtmp, 0.001D0, 10.0D0, &
               'F-value for first wall coolant temp rise')
       case ('fgamcd')
          call parse_real_variable('fgamcd', fgamcd, 0.001D0, 10.0D0, &
               'F-value for current drive gamma')
       case ('fipir')
          call parse_real_variable('fipir', fipir, 0.001D0, 10.0D0, &
               'F-value for Ip/Irod')
       case ('fjohc')
          call parse_real_variable('fjohc', fjohc, 0.001D0, 10.0D0, &
               'F-value for Central Solenoid current at EOF')
       case ('fjohc0')
          call parse_real_variable('fjohc0', fjohc0, 0.001D0, 10.0D0, &
               'F-value for Central Solenoid current at BOP')
       case ('fjtfc')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'FJTFC is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('fhldiv')
          call parse_real_variable('fhldiv', fhldiv, 0.001D0, 10.0D0, &
               'F-value for divertor heat load')
       case ('fflutf')
          call parse_real_variable('fflutf', fflutf, 0.001D0, 10.0D0, &
               'F-value for neutron fluence on TF coil')
       case ('ffuspow')
          call parse_real_variable('ffuspow', ffuspow, 0.001D0, 10.0D0, &
               'F-value for maximum fusion power')
       case ('fiooic')
          call parse_real_variable('fiooic', fiooic, 0.001D0, 10.0D0, &
               'F-value for SCTF iop/icrit')
       case ('fjprot')
          call parse_real_variable('fjprot', fjprot, 0.001D0, 10.0D0, &
               'F-value for SCTF winding pack J')
       case ('flhthresh')
          call parse_real_variable('flhthresh', flhthresh, 0.001D0, 1.0D6, &
               'F-value for L-H power threshold')
       case ('fmva')
          call parse_real_variable('fmva', fmva, 0.001D0, 10.0D0, &
               'F-value for maximum MVA')
       case ('fnbshinef')
          call parse_real_variable('fnbshinef', fnbshinef, 0.001D0, 10.0D0, &
               'F-value for maximum NBI shine-through fraction')
       case ('fpeakb')
          call parse_real_variable('fpeakb', fpeakb, 0.001D0, 10.0D0, &
               'F-value for max toroidal field')
       case ('fpinj')
          call parse_real_variable('fpinj', fpinj, 0.001D0, 10.0D0, &
               'F-value for injection power')
       case ('fpnetel')
          call parse_real_variable('fpnetel', fpnetel, 0.001D0, 10.0D0, &
               'F-value for net electric power')
       case ('fportsz')
          call parse_real_variable('fportsz', fportsz, 0.001D0, 10.0D0, &
               'F-value for port size')
       case ('fpdivlim')
          call parse_real_variable('fpdivlim', fpdivlim, 0.001D0, 1.0D0, &
               'F-value for minimum pdivt')
       case ('ftoroidalgap')
          call parse_real_variable('ftoroidalgap', ftoroidalgap, 0.001D0, 10.0D0, &
                'F-value for toroidal gap consistency')
       case ('f_avspace')
          call parse_real_variable('f_avspace', f_avspace, 0.001D0, 10.0D0, &
                'F-value for radial build consistency (stellarators)')
       case ('fpsepr')
          call parse_real_variable('fpsepr', fpsepr, 0.001D0, 10.0D0, &
               'F-value for Psep/R limit')
       case ('fptemp')
          call parse_real_variable('fptemp', fptemp, 0.001D0, 10.0D0, &
               'F-value for peak centrepost temperature')
       case ('fptfnuc')
          call parse_real_variable('fptfnuc', fptfnuc, 0.001D0, 10.0D0, &
               'F-value for max TF coil nuclear heating')
       case ('fq')
          call parse_real_variable('fq', fq, 0.001D0, 10.0D0, &
               'F-value for edge safety factor')
       case ('fqval')
          call parse_real_variable('fqval', fqval, 0.001D0, 10.0D0, &
               'F-value for fusion gain Q')
       case ('fradpwr')
          call parse_real_variable('fradpwr', fradpwr, 0.0D0, 1.0D0, &
               'F-value for radiation power limit')
       case ('fradwall')
          call parse_real_variable('fradwall', fradwall, 0.001D0, 1.0D0, &
               'f-value for upper limit on radiation wall load')
       case ('freinke')
          call parse_real_variable('freinke', freinke, 0.001D0, 1.0D0, &
               'f-value for upper limit on Reinke detachment criterion')
       case ('frminor')
          call parse_real_variable('frminor', frminor, 0.001D0, 10.0D0, &
               'F-value for minor radius limit')
       case ('fstrcase')
          call parse_real_variable('fstrcase', fstrcase, 0.001D0, 10.0D0, &
               'F-value for TF coil case stress')
       case ('fstrcond')
          call parse_real_variable('fstrcond', fstrcond, 0.001D0, 10.0D0, &
               'F-value for TF coil conduit stress')
       case ('ftaucq')
          call parse_real_variable('ftaucq', ftaucq, 0.001D0, 1.0D0, &
               'F-value for calculated quench time limit')
       case ('ftbr')
          call parse_real_variable('ftbr', ftbr, 0.001D0, 10.0D0, &
               'F-value for tritium breeding ratio limit')
       case ('ftburn')
          call parse_real_variable('ftburn', ftburn, 0.001D0, 10.0D0, &
               'F-value for burn time limit')
       case ('ftcycl')
          call parse_real_variable('ftcycl', ftcycl, 0.001D0, 10.0D0, &
               'F-value for cycle time')
       case ('ftmargtf')
          call parse_real_variable('ftmargtf', ftmargtf, 0.001D0, 10.0D0, &
               'F-value for TF coil temp. margin')
       case ('ftmargoh')
          call parse_real_variable('ftmargoh', ftmargoh, 0.001D0, 10.0D0, &
               'F-value for TF coil temp. margin')

       case ('ftohs')
          call parse_real_variable('ftohs', ftohs, 0.001D0, 10.0D0, &
               'F-value for plasma current ramp-up time')
       case ('ftpeak')
          call parse_real_variable('ftpeak', ftpeak, 0.001D0, 10.0D0, &
               'F-value for peak first wall temperature')
       case ('fvdump')
          call parse_real_variable('fvdump', fvdump, 0.001D0, 10.0D0, &
               'F-value for dump voltage')
       case ('fvs')
          call parse_real_variable('fvs', fvs, 0.001D0, 10.0D0, &
               'F-value for startup V-s requirement')
       case ('fvssu')
         call parse_real_variable('fvssu', fvssu, 0.001D0, 10.0D0, &
               'F-value for start up V-s requirement and availability equality')
       case ('fvvhe')
          call parse_real_variable('fvvhe', fvvhe, 0.001D0, 10.0D0, &
               'F-value for VV He concentration limit')
       case ('fwalld')
          call parse_real_variable('fwalld', fwalld, 0.001D0, 10.0D0, &
               'F-value for wall load limit')
       case ('fzactual')
          call parse_real_variable('fzactual', fzactual, 0.0D0, 1.0D0, &
               'fraction of specified impurity in SOL when constrained by Reinke criteria')
       case ('fzeffmax')
          call parse_real_variable('fzeffmax', fzeffmax, 0.001D0, 1.0D0, &
               'f-value for Zeff limit equation')
       case ('fpoloidalpower')
          call parse_real_variable('fpoloidalpower', fpoloidalpower, 0.001D0, 1.0D0, &
               'f-value for constraint on rate of change of energy in poloidal field')
       case ('fpsep')
           call parse_real_variable('fpsep', fpsep, 0.001D0, 1.0D0, &
                        'f-value to ensure separatrix power is less than value from Kallen bach divertor')
       case ('fpsepbqar')
          call parse_real_variable('fpsepbqar', fpsepbqar, 0.001D0, 1.0D0, &
                       'f-value for TF coil quench temperature < tmax_croco (constraint equation 74)')
       case ('fcqt')
          call parse_real_variable('fcqt', fcqt, 0.001D0, 1.0D0, &
                       'TF coil quench temparature remains below tmax_croco')
       case ('fne0')
          call parse_real_variable('fne0', fne0, 0.001D0, 1.0D0, &
                       'Central electron temperature remains higher that the pedestal one')
       case ('gammax')
          call parse_real_variable('gammax', gammax, 0.01D0, 10.0D0, &
               'Maximum current drive gamma (A/W-m2)')
       case ('maxradwallload')
          call parse_real_variable('maxradwallload', maxradwallload, 0.1D0, 10.0D0, &
               'Maximum permitted radiation wall load (MW/m^2)')
       case ('mvalim')
          call parse_real_variable('mvalim', mvalim, 0.0D0, 1000.0D0, &
               'Maximum MVA limit')
       case ('nbshinefmax')
          call parse_real_variable('nbshinefmax', nbshinefmax, 1.0D-20, 1.0D-1, &
               'Maximum NB shine-through fraction')
       case ('nflutfmax')
          call parse_real_variable('nflutfmax', nflutfmax, 1.0D22, 1.0D24, &
               'Max fast neutron fluence on TF coil (n/m2)')
       case ('pdivtlim')
          call parse_real_variable('pdivtlim', pdivtlim, 0.1D0, 1.0D3, &
               'Minimum pdivt (MW) (con. 80, itvar. 153)')
       case ('peakfactrad')
          call parse_real_variable('peakfactrad', peakfactrad, 0.1D0, 10D0, &
               'peaking factor for radiation wall load')
       case ('pnetelin')
          call parse_real_variable('pnetelin', pnetelin, 1.0D0, 1.0D4, &
               'Required net electric power (MW)')
       case ('powfmax')
          call parse_real_variable('powfmax', powfmax, 1.0D0, 1.0D4, &
               'Maximum fusion power (MW)')
       case ('psepbqarmax')
          call parse_real_variable('psepbqarmax', psepbqarmax, 1.0D0, 50.0D0, &
               'Maximum Psep*Bt/q*A*R ratio (MW.T/m)')
       case ('pseprmax')
          call parse_real_variable('pseprmax', pseprmax, 1.0D0, 50.0D0, &
               'Maximum Psep/R ratio (MW/m)')
       case ('ptfnucmax')
          call parse_real_variable('ptfnucmax', ptfnucmax, 1.0D-6, 1.0D0, &
               'Maximum TF coil nuclear heating (MW/m3)')
       case ('tbrmin')
          call parse_real_variable('tbrmin', tbrmin, 0.001D0, 2.0D0, &
               'Minimum tritium breeding ratio')
       case ('tbrnmn')
          call parse_real_variable('tbrnmn', tbrnmn, 1.0D-3, 1.0D6, &
               'Minimum burn time (s)')
       case ('tcycmn')
          call parse_real_variable('tcycmn', tcycmn, 1.0D-3, 2.0D6, &
               'Minimum cycle time (s)')
       case ('vvhealw')
          call parse_real_variable('vvhealw', vvhealw, 0.01D0, 10.0D0, &
               'Allowable maximum He conc. in VV (appm)')
       case ('walalw')
          call parse_real_variable('walalw', walalw, 0.001D0, 50.0D0, &
               'Allowable wall load (MW/m2)')
       case ('zeffmax')
          call parse_real_variable('zeffmax', zeffmax, 1.0D0, 10.0D0, &
               'Allowable Zeff')

          !  PLASMOD 1D transport model settings

    !Derived type numerics_transp
       case ('plasmod_tol')
          call parse_real_variable('plasmod_tol', plasmod_tol, 0.0D0, 1.0D4, &
               'Tolerance to be reached, in % variation at each time step')
       case ('plasmod_dtmin')
          call parse_real_variable('plasmod_dtmin', plasmod_dtmin, 0.0D0, 1.0D4, &
               'Min time step')
       case ('plasmod_dtmax')
          call parse_real_variable('plasmod_dtmax', plasmod_dtmax, 0.0D0, 1.0D4, &
               'Max time step')
       case ('plasmod_dt')
          call parse_real_variable('plasmod_dt', plasmod_dt, 0.0D0, 1.0D4, &
               'Time step')
       case ('plasmod_dtinc')
          call parse_real_variable('plasmod_dtinc', plasmod_dtinc, 0.0D0, 10.0D4, &
               'Decrease of dt')
       case ('plasmod_ainc')
          call parse_real_variable('plasmod_ainc', plasmod_Ainc, 0.0D0, 2.0D4, &
               'Increase of dt')
       case ('plasmod_test')
          call parse_real_variable('plasmod_test', plasmod_test, 0.0D0, 1.0D6, &
               'Max iteration number')
       case ('plasmod_tolmin')
          call parse_real_variable('plasmod_tolmin', plasmod_tolmin, 0.0D0, 20.0D4, &
               'Multiplier of etolm that should not be overcome')
       case ('plasmod_eopt')
          call parse_real_variable('plasmod_eopt', plasmod_eopt, 0.0D0, 1.0D4, &
               'Exponent of jipperdo')
       case ('plasmod_dtmaxmin')
          call parse_real_variable('plasmod_dtmaxmin', plasmod_dtmaxmin, 0.0D0, 1.0D4, &
               'Exponent of jipperdo2')
       case ('plasmod_dtmaxmax')
          call parse_real_variable('plasmod_dtmaxmax', plasmod_dtmaxmax, 0.0D0, 1.0D4, &
               'Stabilizing coefficient')
       case ('plasmod_capa')
          call parse_real_variable('plasmod_capa', plasmod_capA, 0.0D0, 1.0D4, &
               'First radial grid point')
       case ('plasmod_maxa')
          call parse_real_variable('plasmod_maxa', plasmod_maxA, 0.0D0, 1.0D4, &
               'Diagz 0 or 1')
       case ('plasmod_dgy')
          call parse_real_variable('plasmod_dgy', plasmod_dgy, 0.0D0, 1.0D4, &
               'Newton differential')
       case ('plasmod_iprocess')
          call parse_int_variable('plasmod_iprocess', plasmod_iprocess, 0, 1, &
               '0 - - use PLASMOD functions, 1 - use PROCESS functions')
       case ('plasmod_i_modeltype')
          call parse_int_variable('plasmod_i_modeltype', plasmod_i_modeltype, 0, 10000, &
               '1 - Simple gyrobohm scaling with imposed H factor > 1, other models with H in output')
       case ('plasmod_i_equiltype')
          call parse_int_variable('plasmod_i_equiltype', plasmod_i_equiltype, 1, 20000, &
               '1 - EMEQ, solve equilibrium with given q95, with sawteeth. 2- EMEQ, solve with given Ip, with sawteeth.')
       case ('plasmod_isawt')
          call parse_int_variable('plasmod_isawt', plasmod_isawt, 0, 2, &
               '0 - no sawteeth, 1 - solve with sawteeth.')
       case ('plasmod_nx')
          call parse_int_variable('plasmod_nx', plasmod_nx, 0, 10000, &
               'Number of interpolated grid points')
       case ('plasmod_nxt')
          call parse_int_variable('plasmod_nxt', plasmod_nxt, 0, 10000, &
               'Number of reduced grid points')
       case ('plasmod_nchannels')
          call parse_int_variable('plasmod_nchannels', plasmod_nchannels, 3, 3, &
               'Leave this at 3')
       case ('plasmod_i_impmodel')
          call parse_int_variable('plasmod_i_impmodel', plasmod_i_impmodel, 0, 10000, &
               'Impurity model: 0 - fixed concentration, 1 - concentration fixed at pedestal top, then fixed density.')

   !Derived type composition
       case ('plasmod_globtau')
          call parse_real_array('plasmod_globtau', plasmod_globtau, isub1, 5, &
               'Tauparticle/tauE for D, T, He, Xe, Ar', icode)
        case ('plasmod_psepplh_sup')
          call parse_real_variable('plasmod_psepplh_sup', plasmod_psepplh_sup, 0.0D0, 2.0D4, &
               'Psep/PLH if above this, use Xe')

       case ('plasmod_qdivt')
          call parse_real_variable('plasmod_qdivt', plasmod_qdivt, 0.0D0, 1.0D6, &
               'Divertor heat flux in MW/m^2, if 0, dont use SOL model')
       case ('plasmod_imptype')
          call parse_int_array('plasmod_imptype', plasmod_imptype, isub1, 3, &
               'Impurities: 1 - intrinsic, 2 - Psep control, 3 - seeding for SOL (defaults: W, Xe, Ar)', icode)

    !Derived type inputs
       case ('plasmod_qnbi_psepfac')
          call parse_real_variable('plasmod_qnbi_psepfac', plasmod_qnbi_psepfac, 0.0D0, 1.0D4, &
               'dqnbi/d(1-Psep/PLH)')
       case ('plasmod_cxe_psepfac')
          call parse_real_variable('plasmod_cxe_psepfac', plasmod_cxe_psepfac, 0.0D0, 1.0D4, &
               'dcxe/d(1-Psep/PLH)')
       case ('plasmod_car_qdivt')
          call parse_real_variable('plasmod_car_qdivt', plasmod_car_qdivt, 0.0D0, 1.0D4, &
               'dcar/d(qdivt)')
       case ('plasmod_maxpauxor')
          call parse_real_variable('plasmod_maxpauxor', plasmod_maxpauxor, 0.0D0, 1.0D4, &
               'Max allowed auxiliary power / R')
          !deposition locations
       case ('plasmod_x_heat')
          call parse_real_array('plasmod_x_heat', plasmod_x_heat, isub1, 2, &
               'Element 1 - nbi, element 2 - ech', icode)
       case ('plasmod_x_cd')
          call parse_real_array('plasmod_x_cd', plasmod_x_cd, isub1, 2, &
               'Element 1 - nbi, element 2 - ech', icode)
       case ('plasmod_x_fus')
          call parse_real_array('plasmod_x_fus', plasmod_x_fus, isub1, 2, &
               'Element 1 - nbi, element 2 - ech', icode)
       case ('plasmod_x_control')
          call parse_real_array('plasmod_x_control', plasmod_x_control, isub1, 2, &
               'Element 1 - nbi, element 2 - ech', icode)
       case ('plasmod_dx_heat')
          call parse_real_array('plasmod_dx_heat', plasmod_dx_heat, isub1, 2, &
               'Element 1 - nbi, element 2 - ech', icode)
       case ('plasmod_dx_cd')
          call parse_real_array('plasmod_dx_cd', plasmod_dx_cd, isub1, 2, &
               'Element 1 - nbi, element 2 - ech', icode)
       case ('plasmod_dx_fus')
          call parse_real_array('plasmod_dx_fus', plasmod_dx_fus, isub1, 2, &
               'Element 1 - nbi, element 2 - ech', icode)
       case ('plasmod_dx_control')
          call parse_real_array('plasmod_dx_control', plasmod_dx_control, isub1, 2, &
               'Element 1 - nbi, element 2 - ech', icode)
       case ('plasmod_nbi_energy')
          call parse_real_variable('plasmod_nbi_energy', plasmod_nbi_energy, 0.0D0, 1.0D4, &
               'in keV')
       case ('plasmod_v_loop')
          call parse_real_variable('plasmod_v_loop', plasmod_v_loop, -1.0D4, 1.0D4, &
               'Target loop voltage. If lower than -1.e5 do not use')
       case ('plasmod_pfus')
          call parse_real_variable('plasmod_pfus', plasmod_pfus, 0.0D0, 1.0D4, &
               'If 0. not used (otherwise controlled with Pauxheat)')
       case ('plasmod_fcdp')
          call parse_real_variable('plasmod_fcdp', plasmod_fcdp, -1.0D0, 1.0D0, &
               '(P_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD power over available power')
       case ('plasmod_fradc')
          call parse_real_variable('plasmod_fradc', plasmod_fradc, -1.0D0, 1.0D0, &
               'Pline_Xe / (Palpha + Paux - PlineAr - Psync - Pbrad)')
       case ('plasmod_contrpovs')
          call parse_real_variable('plasmod_contrpovs', plasmod_contrpovs, 0.0D0, 1.0D4, &
               'control power in Paux/lateral_area (MW/m2)')
       case ('plasmod_contrpovr')
          call parse_real_variable('plasmod_contrpovr', plasmod_contrpovr, 0.0D0, 1.0D4, &
               'control power in Paux/R (MW/m)')
       case ('plasmod_eccdeff')
          call parse_real_variable('plasmod_eccdeff', plasmod_eccdeff, 0.0D0, 1.0D0, &
               'current drive multiplier: CD = eccdeff*PCD*TE/NE (not in use yet)')
       case ('plasmod_pech')
          call parse_real_variable('plasmod_pech', plasmod_pech, 0.0D0, 1.0D4, &
               'ech power (not in use yet)')
       case ('plasmod_gamcdothers')
          call parse_real_variable('plasmod_gamcdothers', plasmod_gamcdothers, 0.0D0, 1.0D0, &
               'efficiency multiplier for non-CD heating. If 0.0 pheat treated as if it had no current drive associated')
       case ('plasmod_chisawpos')
          call parse_real_variable('plasmod_chisawpos', plasmod_chisawpos, -10.0D0, 10.0D0, &
               'position where artificial sawtooth diffusivity is added, -1 - uses q=1 position')
       case ('plasmod_chisaw')
          call parse_real_variable('plasmod_chisaw', plasmod_chisaw, 0.0D0, 1.0D4, &
               'artificial diffusivity in m^2/s')
       case ('plasmod_sawpertau')
          call parse_real_variable('plasmod_sawpertau', plasmod_sawpertau, 0.0D0, 1.0D0, &
               'ratio between sawtooth period and confinement time')
       case ('plasmod_spellet')
          call parse_real_variable('plasmod_spellet', plasmod_spellet, 0.0D0, 1.0D4, &
               'pellet mass in units of D in 10^19')
       case ('plasmod_fpellet')
          call parse_real_variable('plasmod_fpellet', plasmod_fpellet, 0.0D0, 1.0D4, &
               'pellet frequency in Hz')

          !Derived type pedestal
       case ('plasmod_pedscal')
          call parse_real_variable('plasmod_pedscal', plasmod_pedscal, 0.0D0, 1.0D1, &
               'multiplication factor of the pedestal scaling in PLASMOD')


          !  Current drive settings

       case ('beamwd')
          call parse_real_variable('beamwd', beamwd, 0.001D0, 5.0D0, &
               'Beam width (m)')

       case ('bscfmax')
          call parse_real_variable('bscfmax', bscfmax, -0.999D0, 0.999D0, &
               '(-fixed)/maximum Bootstrap fraction')
       case ('cboot')
          call parse_real_variable('cboot', cboot, 0.0D0, 10.0D0, &
               'Bootstrap current fraction multiplier')
       case ('enbeam')
          call parse_real_variable('enbeam', enbeam, 1.0D0, 1.0D6, &
               'Neutral beam energy (keV)')
       case ('etalh')
          call parse_real_variable('etalh', etalh, 0.0D0, 1.0D0, &
               'LH wall plug to plasma efficiency')
       case ('etaech')
          call parse_real_variable('etaech', etaech, 0.0D0, 1.0D0, &
               'ECH wall plug to injector efficiency')
       case ('etanbi')
          call parse_real_variable('etanbi', etanbi, 0.0D0, 1.0D0, &
               'NBI wall plug to injector efficiency')
       case ('feffcd')
          call parse_real_variable('feffcd', feffcd, 0.0D0, 20.0D0, &
               'Current drive efficiency fiddle factor')
       case ('forbitloss')
          call parse_real_variable('forbitloss', forbitloss, 0.0D0, 0.999D0, &
               'NBI power orbit loss fraction')
       case ('frbeam')
          call parse_real_variable('frbeam', frbeam, 0.5D0, 2.0D0, &
               'R_tan / R_major for NBI')
       case ('ftritbm')
          call parse_real_variable('ftritbm', ftritbm, 0.0D0, 1.0D0, &
               'Tritium fraction of beam')
       case ('gamma_ecrh')
          call parse_real_variable('gamma_ecrh', gamma_ecrh, 0.0D0, 1.0D0, &
               'User input ECRH gamma_CD')
       case ('rho_ecrh')
          call parse_real_variable('rho_ecrh', rho_ecrh, 0.0D0, 1.0D0, &
               'normalised minor radius at which electron cyclotron current drive is maximum')

       case ('iefrf')
          call parse_int_variable('iefrf', iefrf, 1, 11, &
               'Switch for curr drive efficiency model')
       case ('iefrffix')
          call parse_int_variable('iefrffix', iefrffix, 0, 11, &
               'Switch for 2nd curr drive efficiency model')
       case ('irfcd')
          call parse_int_variable('irfcd', irfcd, 0, 1, &
               'Switch for current drive calculation')
       case ('nbshield')
          call parse_real_variable('nbshield', nbshield, 0.01D0, 0.5D0, &
               'Wall thickness of neutral beam duct (m)')
       case ('pheat')
          call parse_real_variable('pheat', pheat, 0.0D0, 1.0D3, &
               'Heating power not used for C.D. (MW)')
       case ('pheatfix')
          call parse_real_variable('pheatfix', pheatfix, 0.0D0, 1.0D3, &
               'Secondary fixed heating power not used for C.D. (MW)')
       case ('pinjalw')
          call parse_real_variable('pinjalw', pinjalw, 0.0D0, 1.0D3, &
               'Maximum allowed injection power (MW)')
       case ('pinjfixmw')
          call parse_real_variable('pinjfixmw', pinjfixmw, 0.0D0, 1.0D3, &
               'Secondary auxiliary injection power (MW)')
       case ('tbeamin')
          call parse_real_variable('tbeamin', tbeamin, 0.0D0, 10.0D0, &
               'No of NB decay lengths to plas centre')

          !  Time settings

       case ('tburn')
          call parse_real_variable('tburn', tburn, 0.0D0, 1.0D8, &
               'Burn time (s)')
       case ('tdwell')
          call parse_real_variable('tdwell', tdwell, 0.0D0, 1.0D8, &
               'Time between burns (s)')
       case ('theat')
          call parse_real_variable('theat', theat, 0.0D0, 1.0D4, &
               'Heating time after current ramp (s)')
       case ('tohs')
          call parse_real_variable('tohs', tohs, 0.0D0, 1.0D4, &
               'Plasma current ramp-up time for current init (s)')
       case ('tohsin')
          call parse_real_variable('tohsin', tohsin, 0.0D0, 1.0D4, &
               'Switch for TOHS calculation')
       case ('tqnch')
          call parse_real_variable('tqnch', tqnch, 0.0D0, 1.0D4, &
               'PF coil shutdown time (s)')
       case ('tramp')
          call parse_real_variable('tramp', tramp, 0.0D0, 1.0D4, &
               'Initial charge time for PF coils (s)')
       case ('pulsetimings')
          call parse_real_variable('pulsetimings', pulsetimings, 0.0D0, 1.0D0, &
               'Pulse timings switch for lpulse=1')

       ! Divertor settings: 2016 Kallenbach model (2016/07/04)

       case ('target_spread')
          call parse_real_variable('target_spread', target_spread, 0.001D0, 1.0D0, &
               'Increase in SOL power fall-off length due to spreading, mapped to OMP [m]')
       case ('lambda_q_omp')
          call parse_real_variable('lambda_q_omp', lambda_q_omp, 0.0001D0, 1.0D0, &
               'SOL power fall-off length at the outer midplane, perpendicular to field [m]')
       case ('lcon_factor')
          call parse_real_variable('lcon_factor', lcon_factor, 1.0D-1, 1.0D1, &
               'Correction factor for connection length from OMP to divertor')
       case ('netau_sol')
          call parse_real_variable('netau_sol', netau_sol, 0.1D0, 1.0D5, &
               'Parameter describing the degree to which local ionisation equilibrium is reached in the SOL. [ms.1e20/m3]')
       case ('kallenbach_switch')
          call parse_int_variable('kallenbach_switch', kallenbach_switch, 0, 1, &
               'Switch to turn on the 1D Kallenbach divertor model (1=on, 0=off)')
       case ('kallenbach_tests')
          call parse_int_variable('kallenbach_tests', kallenbach_tests, 0, 1, &
               'Switch to turn on tests of the 1D Kallenbach divertor model (1=on, 0=off)')
       case ('kallenbach_test_option')
          call parse_int_variable('kallenbach_test_option', kallenbach_test_option, 0, 10, &
               'Switch to choose testing option for the 1D Kallenbach divertor model')
       case ('kallenbach_scan_switch')
          call parse_int_variable('kallenbach_scan_switch', kallenbach_scan_switch, 0, 1, &
               'Switch to turn on scan of the 1D Kallenbach divertor model (1=on, 0=off)')
       case ('kallenbach_scan_var')
          call parse_int_variable('kallenbach_scan_var', kallenbach_scan_var, 0, 10, &
               'Scan parameter for kallenbach test scan')
       case ('kallenbach_scan_start')
          call parse_real_variable('kallenbach_scan_start', kallenbach_scan_start, 1.0D-10, 1.0D30, &
               'Starting value for kallenbach scan')
       case ('kallenbach_scan_end')
          call parse_real_variable('kallenbach_scan_end', kallenbach_scan_end, 1.0D-10, 1.0D30, &
               'End value for kallenbach scan')
       case ('kallenbach_scan_num')
          call parse_int_variable('kallenbach_scan_step', kallenbach_scan_num, 1, 1000, &
               'Number of scan points for kallenbach scan')
       case ('targetangle')
          call parse_real_variable('targetangle', targetangle, 0.1D0, 90.0D0, &
               'Angle between field-line and divertor target (degrees)')
       case ('ttarget')
          call parse_real_variable('ttarget', ttarget, 1.0D0, 1.0D4, &
               'Plasma temperature adjacent to divertor sheath [eV]')
       case ('qtargettotal')
          call parse_real_variable('qtargettotal', qtargettotal, 0.001D0, 1.0D8, &
               'Power density on target including surface recombination [W/m2]')
       !case ('impurity_enrichment')
       !  call parse_real_variable('impurity_enrichment', impurity_enrichment, 0.1D0, 20.0D0, &
       !       'Ratio of impurity concentrations in SOL to confined plasma')

       case ('impurity_enrichment')
          call parse_real_array('impurity_enrichment', impurity_enrichment, isub1, 14, &
          'Ratio of each impurity concentration in SOL to confined plasma', icode)

       case ('fractionwidesol')
          call parse_real_variable('fractionwidesol', fractionwidesol, 0.001D0, 0.99D0, &
               'Distance from target at which SOL gets broader as a fraction of connection length')

        case ('abserr_sol')
          call parse_real_variable('abserr_sol', abserr_sol, 0.D0, 1.D-1, &
                   'Absolute contribution to the error tolerance in the Kallenbach divertor model')
        case ('relerr_sol')
          call parse_real_variable('relerr_sol', relerr_sol, 0.D0, 1.D-1, &
                  'Relative contribution to the error tolerance in the Kallenbach divertor model')
        if((abserr_sol<1.d-6).and.(relerr_sol<1.d-6))write(*,*)'abserr_sol and relerr_sol must not both be very small.'

      case ('mach0')
      call parse_real_variable('mach0', mach0, 0.D0, 1.D0, &
              'Mach number at target (must be just less than 1)')

       ! See HTS coil module for PROCESS.docx
       !case ('cable_helium_fraction')
       !  call parse_real_variable('cable_helium_fraction', cable_helium_fraction, 0.215D0, 0.99D0, &
       !           'Helium area as a fraction of the cable space.')

          !  Divertor settings

       case ('anginc')
          call parse_real_variable('anginc', anginc, 0.0D0, 1.5707D0, &
               'Field line ang of incid on dvrtr (rad)')
       case ('betai')
          call parse_real_variable('betai', betai, 0.0D0, 1.5707D0, &
               'Poloidal plane angle between inner divertor leg and plate (rad)')
       case ('betao')
          call parse_real_variable('betao', betao, 0.0D0, 1.5707D0, &
               'Poloidal plane angle between outer divertor leg and plate (rad)')
       case ('bpsout')
          call parse_real_variable('bpsout', bpsout, 0.0D0, 10.0D0, &
               'Ref B_p at outboard divertor strike point')
       case ('c1div')
          call parse_real_variable('c1div', c1div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('c2div')
          call parse_real_variable('c2div', c2div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('c3div')
          call parse_real_variable('c3div', c3div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('c4div')
          call parse_real_variable('c4div', c4div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('c5div')
          call parse_real_variable('c5div', c5div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('c6div')
          call parse_real_variable('c6div', c6div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('delld')
          call parse_real_variable('delld', delld, 0.1D0, 2.0D0, &
               'Coefficient for power distribution')
       case ('divclfr')
          call parse_real_variable('divclfr', divclfr, 0.0D0, 1.0D0, &
               'Divertor coolant fraction')
       case ('divdens')
          call parse_real_variable('divdens', divdens, 0.1D0, 1.0D5, &
               'Divertor structure density (kg/m3)')
       case ('divdum')
          call parse_int_variable('divdum', divdum, 0, 1, &
               'Switch for divertor Zeff value')
       case ('divfix')
          call parse_real_variable('divfix', divfix, 0.1D0, 5.0D0, &
               'Divertor structure vertical extent (m)')
       case ('divplt')
          call parse_real_variable('divplt', divplt, 0.01D0, 1.0D0, &
               'Divertor plate thickness (m)')
       case ('fdfs')
          call parse_real_variable('fdfs', fdfs, 0.0D0, 20.0D0, &
               'Radial gradient ratio')
       case ('fdiva')
          call parse_real_variable('fdiva', fdiva, 0.1D0, 2.0D0, &
               'Divertor area fiddle factor')
       case ('fgamp')
          call parse_real_variable('fgamp', fgamp, -100.0D0, 100.0D0, &
               'Sheath potential factor')
       case ('fififi')
          call parse_real_variable('fififi', fififi, 1.0D-6, 1.0D0, &
               'Coefficient for gamdiv')
       case ('frrp')
          call parse_real_variable('frrp', frrp, 0.0D0, 1.0D0, &
               'Fraction of radiated power to plate')
       case ('hldivlim')
          call parse_real_variable('hldivlim', hldivlim, 0.1D0, 20.0D0, &
               'Divertor heat load limit (MW/m2)')
       case ('ksic')
          call parse_real_variable('ksic', ksic, 0.0D0, 2.0D0, &
               'Divertor power fraction thingy')
       case ('omegan')
          call parse_real_variable('omegan', omegan, 0.1D0, 10.0D0, &
               'Pressure ratio (nT)_p / (nT)_s')
       case ('plleni')
          call parse_real_variable('plleni', plleni, 0.1D0, 10.0D0, &
               'Poloidal length, inboard divertor plate (m)')
       case ('plleno')
          call parse_real_variable('plleno', plleno, 0.1D0, 10.0D0, &
               'Poloidal length, outboard divertor plate (m)')
       case ('plsepi')
          call parse_real_variable('plsepi', plsepi, 0.1D0, 10.0D0, &
               'Poloidal length, x to inboard strike point (m)')
       case ('plsepo')
          call parse_real_variable('plsepo', plsepo, 0.1D0, 10.0D0, &
               'Poloidal length, x to outboard strike point (m)')
       case ('prn1')
          call parse_real_variable('prn1', prn1, 0.0D0, 1.0D0, &
               'n_scrapeoff / n_average plasma')
       case ('rlenmax')
          call parse_real_variable('rlenmax', rlenmax, 0.0D0, 1.0D0, &
               'Maximum value for length ratio')
       case ('tdiv')
          call parse_real_variable('tdiv', tdiv, 0.1D0, 100.0D0, &
               'Plasma temperature at divertor (eV)')
       case ('xparain')
          call parse_real_variable('xparain', xparain, 0.01D0, 1.0D4, &
               'Parallel heat transport coeff (m2/s)')
       case ('xpertin')
          call parse_real_variable('xpertin', xpertin, 0.0D0, 10.0D0, &
               'Perpendicular heat trans coeff (m2/s)')
       case ('zeffdiv')
          call parse_real_variable('zeffdiv', zeffdiv, 0.01D0, 100.0D0, &
               'Zeff in the divertor region (if divdum.ne.0)')

          !  Radial / vertical build settings

       case ('aplasmin')
          call parse_real_variable('aplasmin', aplasmin, 0.01D0, 10.0D0, &
               'Minimum minor radius (m)')
       case ('blbmith')
          call parse_real_variable('blbmith', blbmith, 0.0D0, 2.0D0, &
               'Inboard blanket box manifold thickness (m)')
       case ('blbmoth')
          call parse_real_variable('blbmoth', blbmoth, 0.0D0, 2.0D0, &
               'Outboard blanket box manifold thickness (m)')
       case ('blbpith')
          call parse_real_variable('blbpith', blbpith, 0.0D0, 2.0D0, &
               'Inboard blanket back plate thickness (m)')
       case ('blbpoth')
          call parse_real_variable('blbpoth', blbpoth, 0.0D0, 2.0D0, &
               'Outboard blanket back plate thickness (m)')
       case ('blbuith')
          call parse_real_variable('blbuith', blbuith, 0.0D0, 2.0D0, &
               'Inboard blanket breeding unit thickness (m)')
       case ('blbuoth')
          call parse_real_variable('blbuoth', blbuoth, 0.0D0, 2.0D0, &
               'Outboard blanket breeding unit thickness (m)')
       case ('blnkith')
          if (iblanket == 3) then
            !CCFE HCPB model with Tritium Breeding Ratio calculation
            write(outfile,*) '**********'
            write(outfile,*) 'ERROR. BLNKITH input is not required for CCFE HCPB model with Tritium Breeding Ratio calculation -'
            write(outfile,*) 'please remove it from the input file'
            write(outfile,*) '**********'
         else
            call parse_real_variable('blnkith', blnkith, 0.0D0, 10.0D0, &
                'Inboard blanket thickness (m)')
          end if
       case ('blnkoth')
           if (iblanket == 3) then
            !CCFE HCPB model with Tritium Breeding Ratio calculation
            write(outfile,*) '**********'
            write(outfile,*) 'ERROR. BLNKOTH input is not required for CCFE HCPB model with Tritium Breeding Ratio calculation -'
            write(outfile,*) 'please remove it from the input file'
            write(outfile,*) '**********'
          else
            call parse_real_variable('blnkoth', blnkoth, 0.0D0, 10.0D0, &
                'Outboard blanket thickness (m)')
          end if
       case ('blnktth')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'WARNING. BLNKTTH is now always calculated rather than input -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('bcylth')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'ERROR. BCYLTH is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('bore')
          call parse_real_variable('bore', bore, 0.0D0, 50.0D0, &
               'Machine bore (m)')
       case ('clhsf')
          call parse_real_variable('clhsf', clhsf, 2.0D0, 10.0D0, &
               'Cryostat lid height scaling factor (m)')
       case ('ddwex')
          call parse_real_variable('ddwex', ddwex, 0.0D0, 10.0D0, &
               'cryostat wall thickness (m)')
       case ('ddwi')
          call parse_real_variable('ddwi', ddwi, 0.0D0, 10.0D0, &
               'Vacuum vessel thickness (m)')
       case ('fcspc')
          call parse_real_variable('fcspc', fcspc, 0.0D0, 1.0D0, &
               'Fraction of space occupied by CS pre-comp structure')
       case ('fmsbc')
          call parse_real_variable('fmsbc', fmsbc, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in buck cyl')
       case ('fmsbl')
          call parse_real_variable('fmsbl', fmsbl, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in blanket')
       case ('fmsdwe')
          call parse_real_variable('fmsdwe', fmsdwe, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in cryostat')
       case ('fmsdwi')
          call parse_real_variable('fmsdwi', fmsdwi, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in vacuum vessel')
       case ('fmsfw')
          call parse_real_variable('fmsfw', fmsfw, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in first wall')
       case ('fmsoh')
          call parse_real_variable('fmsoh', fmsoh, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in Central Solenoid')
       case ('fmssh')
          call parse_real_variable('fmssh', fmssh, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in shield')
       case ('fmstf')
          call parse_real_variable('fmstf', fmstf, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in TF coil')
       case ('fseppc')
          call parse_real_variable('fseppc', fseppc, 1.0D6, 1.0D9, &
               'CS separation force held by CS pre-comp structure')
       case ('oh_steel_frac')
          call parse_real_variable('oh_steel_frac', oh_steel_frac, 1.0D-3, 0.999D0, &
               'Central solenoid steel fraction')
       case ('foh_stress')
          call parse_real_variable('foh_stress', foh_stress, 1.0D-3, 1.0D0, &
               'F-value for CS coil Tresca stress limit')
!       case ('fwith')
!          call parse_real_variable('fwith', fwith, 0.0D0, 10.0D0, &
!               'Inboard first wall thickness, initial estimate (m)')
!       case ('fwoth')
!          call parse_real_variable('fwoth', fwoth, 0.0D0, 10.0D0, &
!               'Outboard first wall thickness, initial estimate (m)')
       case ('gapoh')
          call parse_real_variable('gapoh', gapoh, 0.0D0, 10.0D0, &
               'Gap between OHC and TF coil (m)')
       case ('gapds')
          call parse_real_variable('gapds', gapds, 0.0D0, 10.0D0, &
               'Gap between inboard vacuum vessel and shield (m)')
       case ('gapomin')
          call parse_real_variable('gapomin', gapomin, 0.0D0, 10.0D0, &
               'Min gap between outboard shield & vac vessel (m)')
       case ('iohcl')
          call parse_int_variable('iohcl', iohcl, 0, 1, &
               'Switch for existence of Central Solenoid')
       case ('iprecomp')
          call parse_int_variable('iprecomp', iprecomp, 0, 1, &
               'Switch for existence of Central Solenoid pre-compression structure')
       case ('ohcth')
          call parse_real_variable('ohcth', ohcth, 0.0D0, 10.0D0, &
               'Central Solenoid thickness (m)')
       case ('rinboard')
          call parse_real_variable('rinboard', rinboard, 0.1D0, 10.0D0, &
               'Plasma inboard radius (m)')
       case ('rpf2dewar')
          call parse_real_variable('rpf2dewar', rpf2dewar, 0.1D0, 5.0D0, &
               'Outer PF coil to cryostat distance (m)')
      case ('r_cp_top')
         call parse_real_variable('r_cp_top', r_cp_top, 0.0010D0, 10.0D0, &
              'Top CP outer radius (ST only) (m)')
       case ('scrapli')
          call parse_real_variable('scrapli', scrapli, 0.0D0, 10.0D0, &
               'Inboard scrapeoff length (m)')
       case ('scraplo')
          call parse_real_variable('scraplo', scraplo, 0.0D0, 10.0D0, &
               'Outboard scrapeoff length (m)')
       case ('shldith')
          call parse_real_variable('shldith', shldith, 0.0D0, 10.0D0, &
               'Inboard shield thickness (m)')
       case ('shldlth')
          call parse_real_variable('shldlth', shldlth, 0.0D0, 10.0D0, &
               'Lower (divertor) shield thickness (m)')
       case ('shldoth')
          call parse_real_variable('shldoth', shldoth, 0.0D0, 10.0D0, &
               'Outboard shield thickness (m)')
       case ('shldtth')
          call parse_real_variable('shldtth', shldtth, 0.0D0, 10.0D0, &
               'Top shield thickness (m)')
       case ('sigallpc')
          call parse_real_variable('sigallpc', sigallpc, 0.0D1, 1.0D9, &
               'Allowable stress in CS pre-comp structure (Pa)')
    ! Issue #514 Make tfcth an output not an input or iteration variable:
    ! Eventually this input will be removed.
       case ('tfcth')
          call parse_real_variable('tfcth', tfcth, 0.0D0, 10.0D0, &
               'TF coil thickness (m)')
       case ('dr_tf_wp')
          call parse_real_variable('dr_tf_wp', dr_tf_wp, 0.0D0, 10.0D0, &
               'TF coil winding pack radial thickness (m)')

       case ('tfootfi')
          call parse_real_variable('tfootfi', tfootfi, 0.2D0, 5.0D0, &
               'TFC outboard/inboard leg thickness')
       case ('tftsgap')
          call parse_real_variable('tftsgap', tftsgap, 0.0D0, 5.0D0, &
               'Minimum gap between TF and thermal shield for manufacturing etc. (m)')
       case ('thshield')
          call parse_real_variable('thshield', thshield, 0.0D0, 10.0D0, &
               'TF/VV thermal shield thickness (m)')
       case ('vgap')
          call parse_real_variable('vgap', vgap, 0.0D0, 10.0D0, &
               'Vert gap between x-pnt and divertor (m)')
       case ('vgap2')
          call parse_real_variable('vgap2', vgap2, 0.0D0, 10.0D0, &
               'Vert gap between TF coil and shield (m)')
       case ('vgaptop')
          call parse_real_variable('vgaptop', vgaptop, 0.0D0, 10.0D0, &
               'Top vert gap between plasma and first wall (m)')
       case ('vvblgap')
          call parse_real_variable('vvblgap', vvblgap, 0.0D0, 5.0D0, &
               'Gap between vacuum vessel and blanket (m)')

          !  TF coil settings

       case ('bcritsc')
          call parse_real_variable('bcritsc', bcritsc, 10.0D0, 50.0D0, &
               'Critical field for superconductor')

       !case ('tape_width')
       !   call parse_real_variable('tape_width', tape_width, 0.0D0, 0.1D0, &
       !       'Mean width of HTS tape in CroCo (m)')

       case ('rebco_thickness')
          call parse_real_variable('rebco_thickness', rebco_thickness, 0.01D-6, 100.0D-6, &
               'rebco_thickness')
       case ('hastelloy_thickness')
          call parse_real_variable('hastelloy_thickness', hastelloy_thickness, 0.01D-6, 1000.0D-6, &
               'hastelloy_thickness')
      ! case ('croco_id')
      !   call parse_real_variable('croco_id', croco_id, 0.0D0, 0.1D0, &
      !       'croco_id')
      ! case ('croco_od')
      !    call parse_real_variable('croco_od', croco_od, 0.0D0, 0.1D0, &
      !         'Outer diameter of CroCo strand (m)')
       case ('croco_thick')
          call parse_real_variable('croco_thick', croco_thick, 0.001D0, 0.1D0, &
               'Thickness of CroCo copper tube (m)')
       case ('copper_thick')
          call parse_real_variable('copper_thick', copper_thick, 0.0D0, 1000.0D-6, &
               'copper_thick (m)')
    !    case ('copper_bar')
    !       call parse_real_variable('copper_bar', copper_bar, 0.0D0, 0.9D0, &
    !            'area of central copper bar, as a fraction of area inside the jacket')
       case ('copper_rrr')
          call parse_real_variable('copper_rrr', copper_rrr, 1.0D0, 1.0D4, &
               'residual resistivity ratio copper in TF superconducting cable')

       case ('coppera_m2_max')
          call parse_real_variable('coppera_m2_max', copperA_m2_max, 1.0D6, 1.0D10, &
               'Maximum TF coil current / copper area (A/m2)')
       case ('f_coppera_m2')
          call parse_real_variable('f_coppera_m2', f_coppera_m2, 1.0D-3, 1.0D1, &
               'f-value for constraint 75: TF coil current / copper area < copperA_m2_max')

       case ('casthi')
          call parse_real_variable('casthi', casthi, 0.0D0, 1.0D0, &
               'TF coil case inner thickness (m)')
       ! OR
       case ('casthi_fraction')
          call parse_real_variable('casthi_fraction', casthi_fraction, 0.0D0, 1.0D0, &
               'inboard TF coil case plasma side thickness as a fraction of tfcth')

       ! Use EITHER
       case ('casths')
          call parse_real_variable('casths', casths, 0.0D0, 1.0D0, &
               'TF coil case sidewall thickness (m)')
       ! OR
       case ('casths_fraction')
          call parse_real_variable('casths_fraction', casths_fraction, 0.0D0, 1.0D0, &
               'inboard TF coil sidewall case thickness as a fraction of tftort')

       case ('cpttf')
          call parse_real_variable('cpttf', cpttf, 0.001D0, 1.0D6, &
               'TF coil leg current per turn (A)')

       case ('cpttf_max')
          call parse_real_variable('cpttf_max', cpttf_max, 1.0D0, 1.0D6, &
                    'Maximum allowable TF coil leg current per turn (A) (constraint equation 77)')

       case ('alstrtf')
          call parse_real_variable('alstrtf', alstrtf, 1.0D6, 1.0D11, &
               'Allowable Tresca stress in TF coil structural material (Pa)')
       case ('alstroh')
          call parse_real_variable('alstroh', alstroh, 1.0D6, 1.0D11, &
               'Allowable hoop stress in Central Solenoid structural material (Pa)')

       case ('i_cs_stress')
          call parse_int_variable('i_cs_stress', i_cs_stress, 0, 1, &
               'Switch for CS stress calculation')

       case ('dcase')
          call parse_real_variable('dcase', dcase, 1.0D3, 1.0D5, &
               'Density of TF coil case (kg/m3)')
       case ('dcond')
          call parse_real_array('dcond', dcond, isub1, 6, &
               'TF/PF coil superconductor density (kg/m3)', icode)
       case ('dcondins')
          call parse_real_variable('dcondins', dcondins, 5.0D2, 1.0D4, &
               'Density of TF coil insulation (kg/m3)')
       case ('dcopper')
          call parse_real_variable('dcopper', dcopper, 8.0D3, 1.0D4, &
               'Density of copper (kg/m3)')
       case ('dalu')
          call parse_real_variable('dalu', dalu, 2.5D3, 3.0D4, &
               'Density of Alumium (kg/m3)')
      case ('dhecoil')
          call parse_real_variable('dhecoil', dhecoil, 0.0d0, 0.1d0, &
               'Diameter of He coil in TF winding (m)')
       case ('drtop')
          call parse_real_variable('drtop', drtop, -1.5D0, 1.5D0, &
               'ST CP top radius adjust (m)')
       case ('dztop')
          call parse_real_variable('dztop', dztop, -0.5D0, 0.5D0, &
               'ST CP taper height adjust (m)')
       case ('etapump')
          call parse_real_variable('etapump', etapump, 0.0D0, 1.0D0, &
               'Efficiency of c/p coolant pump')
       case ('eyoung_steel')
          call parse_real_variable('eyoung_steel', eyoung_steel, 1.0D8, 1.0D13, &
               'Steel case Youngs Modulus (Pa)')
       case ('eyoung_ins')
          call parse_real_variable('eyoung_ins', eyoung_ins, 1.0D8, 1.0D13, &
               'Insulator Youngs Modulus (Pa)')
       case ('eyoung_winding')
          call parse_real_variable('eyoung_winding', eyoung_winding, 1.0D8, 1.0D13, &
               'Winding pack Youngs Modulus (Pa)')
       case ('eyoung_al')
          call parse_real_variable('eyoung_al', eyoung_al, 0.0D0, 1.0D0, &
               'Reinforced aluminium Young modulus for TF stress calc.')
       case ('eyoung_res_tf_buck')
          call parse_real_variable('eyoung_res_tf_buck', eyoung_res_tf_buck, 1.0D-10, 1.0D12, &
               'Reinforced aluminium Young modulus for TF stress calc.')
       case ('farc4tf')
          call parse_real_variable('farc4tf', farc4tf, 0.0D0, 1.0D0, &
               'TF coil shape parameter')
       case ('t_crit_nbti')
          call parse_real_variable('t_crit_nbti ', t_crit_nbti , 0.0D0, 15.0D0, &
               'Critical temperature of GL_nbti ')
       case ('b_crit_upper_nbti')
          call parse_real_variable('b_crit_upper_nbti', b_crit_upper_nbti, 0.0D0, 30.0D0, &
                    'Upper critical field of GL_nbti ')
       case ('fcoolcp')
          call parse_real_variable('fcoolcp', fcoolcp, 0.0D0, 1.0D0, &
               'Coolant fraction of TF centrepost (itart=1) or the whole magnet (itart=0)')
       case ('fcoolleg')
          call parse_real_variable('fcoolleg', fcoolleg, 0.0D0, 1.0D0, &
               'Coolant fraction of TF outboard leg (itart=1 only)')
       case ('fcutfsu')
          call parse_real_variable('fcutfsu', fcutfsu, 0.0D0, 1.0D0, &
               'Cu fraction of SCTF cable conductor')
       case ('fhts')
          call parse_real_variable('fhts', fhts, 0.01D0, 1.0D0, &
               'Technology adjustment factor for Bi-2212 HTS')
       case ('frhocp')
          call parse_real_variable('frhocp', frhocp, 0.01D0, 5.0D0, &
               'Centrepost (itart=1) or global (itart=0) resistivity enhancement factor')
       case ('frholeg')
          call parse_real_variable('frholeg', frholeg, 0.01D0, 5.0D0, &
               'TART outboard leg resistivity enhancement factor')
       case ('rho_tf_joints')
          call parse_real_variable('rho_tf_joints', rho_tf_joints, 0.0D0, 1.0D-2, &
               'TF joints surfacic resistivity')
       case ('th_joint_contact')
          call parse_real_variable('th_joint_contact', th_joint_contact, 0.0D0, 1.0D0, &
               'TF sliding joints contact pad width')
       case ('n_tf_joints_contact')
          call parse_int_variable('n_tf_joints_contact', n_tf_joints_contact, 1, 50, &
               'Number of contact per sliding joint')
       case ('n_tf_joints')
          call parse_int_variable('n_tf_joints', n_tf_joints, 1, 50, &
               'Number of joints per turn')
       case ('eff_tf_cryo')
          call parse_real_variable('eff_tf_cryo', eff_tf_cryo, 0.0D0, 1.0D0, &
               'TF coil cryo-plane efficiency')  
       case ('i_tf_plane_stress')
         call parse_int_variable('i_tf_plane_stress', i_tf_plane_stress, 0, 1, &
               'Switch for the TF stress model')
       case ('i_tf_tresca')
          call parse_int_variable('i_tf_tresca', i_tf_tresca, 0, 1, &
                         'Switch for TF coil Tresca criterion')
       case ('i_tf_wp_geom')
          call parse_int_variable('i_tf_wp_geom', i_tf_wp_geom, 0, 2, &
                    'Switch for TF WP geometry selection')
       case ('i_tf_case_geom')
          call parse_int_variable('i_tf_case_geom', i_tf_case_geom, 0, 1, &
                    'Switch for TF case geometry selection')
       case ('i_tf_turns_integer')
          call parse_int_variable('i_tf_turns_integer', i_tf_turns_integer, 0, 1, &
                    'Switch for TF coil integer/non-integer turns')
       case ('i_tf_bucking')
          call parse_int_variable('i_tf_bucking', i_tf_bucking, 0, 3, &
               'Switch for bucking cylinder (case)')
       case ('i_tf_sc_mat')
          call parse_int_variable('i_tf_sc_mat', i_tf_sc_mat, 1, 7, &
               'TF coil superconductor material')
          if (i_tf_sc_mat == 2) then
             write(outfile,*) ' '
             write(outfile,*) '**********'
             write(outfile,*) 'Warning if you are using an old input file:'
             write(outfile,*) 'i_tf_sc_mat=2 usage has changed -'
             write(outfile,*) 'please check validity!'
             write(outfile,*) '**********'
             write(outfile,*) ' '
          end if
       case ('itfmod')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'ITFMOD is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          ! write(outfile,*) 'and replace it with TFC_MODEL'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('i_tf_sup')
          call parse_int_variable('i_tf_sup', i_tf_sup, 0, 2, &
               'Switch for TF coil type')
       case ('i_tf_shape')
         call parse_int_variable('i_tf_shape', i_tf_shape, 0, 2, &
              'Switch for TF coil shape')
       case ('jbus')
          call parse_real_variable('jbus', jbus, 1.0D4, 1.0D8, &
               'TF coil bus current density (A/m2)')
       case ('jcrit_model')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'JCRIT_MODEL is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('jcritsc')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'JCRITSC is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('n_pancake')
          call parse_int_variable('n_pancake', n_pancake, 1, 100, &
               'Number of pancakes in TF coil (i_tf_turns_integer=1)')
       case ('n_layer')
          call parse_int_variable('n_layer', n_layer, 1, 100, &
               'Number of layers in TF coil (i_tf_turns_integer=1)')
       case ('n_tf_graded_layers')
          call parse_int_variable('n_tf_graded_layers', n_tf_graded_layers, 1, 20, &
               'Number of layers of different stress properties in the WP')
       case ('n_rad_per_layer')
          call parse_int_variable('n_rad_per_layer', n_rad_per_layer, 1, 500, &
               'Size of the arrays per layers storing the radial dependent stress ')
       case ('oacdcp')
          call parse_real_variable('oacdcp', oacdcp, 1.0D4, 1.0D9, &
               'Overall J in inboard TF coil midplane')
       case ('poisson_steel')
          call parse_real_variable('poisson_steel', poisson_steel, 0.0D0, 1.0D0, &
               'Steel Poissons ratio for TF stress calc.')
       case ('poisson_copper')
          call parse_real_variable('poisson_copper', poisson_copper, 0.0D0, 1.0D0, &
               'Steel Poissons ratio for TF stress calc.')
       case ('poisson_al')
          call parse_real_variable('poisson_al', poisson_al, 0.0D0, 1.0D0, &
               'Aluminium Poissons ratio for TF stress calc.')
       case ('ptempalw')
          call parse_real_variable('ptempalw', ptempalw, 4.0D0, 573.15D0, &
               'Maximum peak centrepost temp. (K)')
       case ('rcool')
          call parse_real_variable('rcool', rcool, 1.0D-6, 1.0D0, &
               'Centrepost coolant channel radius')
       case ('ripmax')
          call parse_real_variable('ripmax', ripmax, 0.1D0, 100.0D0, &
               'Max allowed ripple ampl. at plasma edge (%)')
       case ('sigvvall')
          call parse_real_variable('sigvvall', sigvvall, 0.1D6, 500.0D6, &
               'Allowable stress in vacuum vessel for TF quench (Pa)')
       case ('strncon_cs')
          call parse_real_variable('strncon_cs', strncon_cs, -0.02D0, 0.02D0, &
               'Strain in CS superconductor material')
       case ('strncon_pf')
          call parse_real_variable('strncon_pf', strncon_pf, -0.02D0, 0.02D0, &
               'Strain in PF superconductor material')
       case ('strncon_tf')
          call parse_real_variable('strncon_tf', strncon_tf, -0.02D0, 0.02D0, &
               'Strain in TF superconductor material')
       case ('tcoolin')
          call parse_real_variable('tcoolin', tcoolin, 4.0D0, 373.15D0, &
               'Centrepost coolant inlet temperature (K)')
       case ('tcpav')
          call parse_real_variable('tcpav', tcpav, 4.0D0, 573.15D0, &
               'Average centrepost coolant temperature (K)')
       case ('tcritsc')
          call parse_real_variable('tcritsc', tcritsc, 1.0D0, 300.0D0, &
               'Critical temperature for superconductor')
       case ('tdmptf')
          call parse_real_variable('tdmptf', tdmptf, 0.1D0, 100.0D0, &
               'Dump time for TF coil (s)')
      !  case ('tfc_model')
      !     call parse_int_variable('tfc_model', tfc_model, 0, 1, &
      !          'Switch for TF coil model')
       case ('tfinsgap')
          call parse_real_variable('tfinsgap', tfinsgap, 1.0D-10, 1.0D-1, &
               'TF coil WP insertion gap (m)')
       case ('rhotfbus')
          call parse_real_variable('rhotfbus', rhotfbus, 0.0D0, 1.0D-5, &
               'TF coil bus (feeders) resistivity (ohm-m)')
       case ('n_tf')
          call parse_real_variable('n_tf', n_tf, 0.0D0, 100.0D0, &
               'Number of TF coils')
       case ('tftmp')
          call parse_real_variable('tftmp', tftmp, 0.01D0, 10.0D0, &
               'Peak TF coil He coolant temp. (K)')      
       case ('t_turn')
          call parse_real_variable('t_turn', t_turn, 0.0D0, 0.1D0, &
               'TF turn square dimensions (m)')
       case ('thicndut')
          call parse_real_variable('thicndut', thicndut, 0.0D0, 0.1D0, &
               'Conduit insulation thickness (m)')
       case ('layer_ins')
          call parse_real_variable('layer_ins', layer_ins, 0.0D0, 0.1D0, &
               'Additional insulation thickness between layers (m)')
       case ('thkcas')
          call parse_real_variable('thkcas', thkcas, 0.0D0, 1.0D0, &
               'External supercond. case thickness (m)')
       case ('thwcndut')
          call parse_real_variable('thwcndut', thwcndut, 0.001D0, 0.1D0, &
               'TF coil conduit case thickness (m)')
       case ('tinstf')
          call parse_real_variable('tinstf', tinstf, 0.0D0, 0.1D0, &
               'Ground wall insulation thickness (m)')
       case ('tmargmin_tf')
          call parse_real_variable('tmargmin_tf', tmargmin_tf, 0.0D0, 20.0D0, &
               'Minimum allowable temp margin: TF coil (K)')
       case ('tmargmin_cs')
          call parse_real_variable('tmargmin_cs', tmargmin_cs, 0.0D0, 20.0D0, &
               'Minimum allowable temp margin: CS (K)')
       case ('tmargmin')
          call parse_real_variable('tmargmin', tmargmin, 0.0D0, 20.0D0, &
               'Minimum allowable temp margin: TFC AND CS (K)')
       case ('tmaxpro')
          call parse_real_variable('tmaxpro', tmaxpro, 0.0D0, 1.0D3, &
               'Maximum temp rise during quench (K)')

       case ('quench_model')
          call parse_string_variable('quench_model', quench_model, 'quench_model')
      case ('quench_detection_ef')
          call parse_real_variable('quench_detection_ef', quench_detection_ef, 0.0D0, 1.0D1, &
               'Electric field at which TF quench is detected and discharge begins (V/m)')

       case ('tmax_croco')
          call parse_real_variable('tmax_croco', tmax_croco, 4.0D0, 1.0D3, &
               'CroCo strand: maximum temp during a quench (K)')
    !    case ('tmax_jacket')
    !       call parse_real_variable('tmax_jacket', tmax_jacket, 4.0D0, 1.0D3, &
    !            'Jacket: maximum temp during a quench (K)')

       case ('tmpcry')
          call parse_real_variable('tmpcry', tmpcry, 0.01D0, 10.0D0, &
               'Cryogenic temperature (K)')
       case ('vcool')
          call parse_real_variable('vcool', vcool, 0.001D0, 100.0D0, &
               'Inlet centrepost coolant speed (m/s)')
       case ('vdalw')
          call parse_real_variable('vdalw', vdalw, 0.0D0, 100.0D0, &
               'Max V across TFC during quench (kV)')
       case ('f_vforce_inboard')
          call parse_real_variable('f_vforce_inboard', f_vforce_inboard, 0.0D0, 1.0D0, &
               'Fraction of vertical force taken by the TF inboard leg')
       case ('vftf')
          call parse_real_variable('vftf', vftf, 0.0D0, 1.0D0, &
               'Coolant fraction of TF coil leg')

       !  PF coil settings

       case ('bmaxcs_lim')
         call parse_real_variable('bmaxcs_lim', bmaxcs_lim, 0.01D0, 100.0D0, &
               'Maximum allowed peak field on central solenoid')
       case ('fbmaxcs')
         call parse_real_variable('fbmaxcs', fbmaxcs, 0.01D0, 1.0D0, &
               'F-value for max peak CS field (con. 79, itvar 149)')

       case ('alfapf')
          call parse_real_variable('alfapf', alfapf, 1.0D-12, 1.0D0, &
               'PF coil current smoothing parameter')
       case ('coheof')
          call parse_real_variable('coheof', coheof, 1.0D4, 5.0D8, &
               'Central Solenoid current density at EOF')
       case ('cptdin')
          call parse_real_array('cptdin', cptdin, isub1, ngc2, &
               'Current per turn for PF coil', icode)
       case ('etapsu')
          call parse_real_variable('etapsu', etapsu, 0.0D0, 1.0D0, &
               'Efficiency of ohmic heating')
       case ('fcohbof')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'FCOHBOF is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('fcohbop')
          call parse_real_variable('fcohbop', fcohbop, 0.0D0, 1.0D0, &
               'Central Solenoid J ratio : BOP/EOF')
       case ('fcuohsu')
          call parse_real_variable('fcuohsu', fcuohsu, 0.0D0, 1.0D0, &
               'Cu frac of conductor in Central Solenoid cable')
       case ('fcupfsu')
          call parse_real_variable('fcupfsu', fcupfsu, 0.0D0, 1.0D0, &
               'Cu fraction of PF cable conductor')
       case ('ipfloc')
          call parse_int_array('ipfloc', ipfloc, isub1, ngc, &
               'PF coil location', icode)
       case ('ipfres')
          call parse_int_variable('ipfres', ipfres, 0, 1, &
               'Switch for supercond / resist PF coils')
       case ('isumatoh')
          call parse_int_variable('isumatoh', isumatoh, 1, 7, &
               'Central Solenoid superconductor material')
       case ('isumatpf')
          call parse_int_variable('isumatpf', isumatpf, 1, 7, &
               'PF coil superconductor material')
       case ('ncls')
          call parse_int_array('ncls', ncls, isub1, ngrpmx, &
               'No of coils in PF group', icode)
       case ('nfxfh')
          call parse_int_variable('nfxfh', nfxfh, 1, nfixmx/2, &
               'Central Solenoid splitting parameter')
       case ('ngrp')
          call parse_int_variable('ngrp', ngrp, 0, ngrpmx, &
               'No of groups of PF coils')
       case ('ohhghf')
          call parse_real_variable('ohhghf', ohhghf, 0.0D0, 2.0D0, &
               'Central Solenoid height / TF coil height')
       case ('pfclres')
          call parse_real_variable('pfclres', pfclres, 0.0D0, 1.0D-4, &
               'PF coil resistivity (ohm-m)')
       case ('rjconpf')
          call parse_real_array('rjconpf', rjconpf, isub1, ngc2, &
               'Average J of PF coil (A/m2)', icode)
       case ('routr')
          call parse_real_variable('routr', routr, -3.0D0, 3.0D0, &
               'Gap from outboard TFC leg for PFC')
       case ('rpf1')
          call parse_real_variable('rpf1', rpf1, 0.0D0, 3.0D0, &
               'Radial offset for group 1 PF coils')
       case ('rpf2')
          call parse_real_variable('rpf2', rpf2, -3.0D0, 3.0D0, &
               'Radial offset for group 2 PF coils')
       case ('sigpfcalw')
          call parse_real_variable('sigpfcalw', sigpfcalw, 1.0D0, 1.0D3, &
               'Allowable stress in the PF coil case (MPa)')
       case ('sigpfcf')
          call parse_real_variable('sigpfcf', sigpfcf, 0.1D0, 1.0D0, &
               'Fraction of JxB force supported by PF coil case')
       case ('vf')
          call parse_real_array('vf', vf, isub1, ngc2, &
               'Void fraction of PF coil', icode)
       case ('vfohc')
          call parse_real_variable('vfohc', vfohc, 0.0D0, 1.0D0, &
               'Central Solenoid void fraction for coolant')
       case ('zref')
          call parse_real_array('zref', zref, isub1, ngrpmx, &
               'height of coil group / minor radius', icode)

       case ('afw')
          call parse_real_variable('afw', afw, 1.0D-3, 0.5D0, &
               'Inner radius of first wall coolant channel (m)')

       case ('fw_wall')
          call parse_real_variable('fw_wall', fw_wall, 0.5D-3, 0.1D0, &
               'wall thickness of first wall coolant channels (m)')
       case ('pitch')
          call parse_real_variable('pitch', pitch, 0.5D-3, 0.1D0, &
               'pitch of first wall cooling channels (m)')
       case ('fwinlet')
          call parse_real_variable('fwinlet', fwinlet, 300.0d0, 1500.0D0, &
               'inlet temperature of first wall coolant (K)')
       case ('fwoutlet')
          call parse_real_variable('fwoutlet', fwoutlet, 300.0d0, 1500.0D0, &
               'outlet temperature of first wall coolant (K)')
       case ('fwpressure')
          call parse_real_variable('fwpressure', fwpressure, 1.0d5, 1.0D8, &
               'first wall coolant pressure (Pa)')
       case ('fwcoolant')
          call parse_string_variable('fwcoolant', fwcoolant, 'first wall coolant')
          call lower_case(fwcoolant)
       case ('roughness')
          call parse_real_variable('roughness', roughness, 0.0d0, 1.0D-2, &
               'first wall channel roughness epsilon')
       case ('fw_channel_length')
          call parse_real_variable('fw_channel_length', fw_channel_length, 1.0D-3, 1.0D3, &
               'first wall channel length')
       case ('peaking_factor')
          call parse_real_variable('peaking_factor', peaking_factor, 1.0d0, 100.0D0, &
               'peaking factor for first wall heat loads')


       case ('bctmp')
          call parse_real_variable('bctmp', bctmp, 1.0D0, 800.0D0, &
               'First wall bulk coolant temperature (C)')
       case ('blpressure')
          call parse_real_variable('blpressure', blpressure, 1.0D5, 1.0D8, &
               'Blanket coolant pressure (Pa)')
       case ('coolp')
          call parse_real_variable('coolp', coolp, 1.0D5, 1.0D8, &
               'blanket coolant pressure (Pa) stellarator ONLY')

       case ('dtstor')
          call parse_real_variable('dtstor', dtstor, 50.0D0, 500.0D0, &
               'Max temp change in thermal storage medium (K)')
       case ('istore')
          call parse_int_variable('istore', istore, 1, 3, &
               'Switch for thermal storage option')
       case ('itcycl')
          call parse_int_variable('itcycl', itcycl, 1, 3, &
               'Switch for 1st wall axial stress model')
       case ('lpulse')
          call parse_int_variable('lpulse', lpulse, 0, 1, &
               'Switch for pulsed reactor model')

          !  First wall, blanket, shield settings

       case ('primary_pumping')
          call parse_int_variable('primary_pumping', primary_pumping, 0, 3, &
               'Switch for pumping of primary coolant')
       case ('htpmw_blkt')
          call parse_real_variable('htpmw_blkt', htpmw_blkt, 0.0D0, 1.0D3, &
               'blanket coolant mechanical pumping power (MW)')
       case ('htpmw_div')
          call parse_real_variable('htpmw_div', htpmw_div, 0.0D0, 1.0D3, &
               'divertor coolant mechanical pumping power (MW)')
       case ('htpmw_fw')
          call parse_real_variable('htpmw_fw', htpmw_fw, 0.0D0, 1.0D3, &
               'first wall coolant mechanical pumping power (MW)')
       case ('htpmw_shld')
          call parse_real_variable('htpmw_shld', htpmw_shld, 0.0D0, 1.0D3, &
               'shield and vacuum vessel coolant mechanical pumping power (MW)')


       case ('secondary_cycle')
          call parse_int_variable('secondary_cycle', secondary_cycle, 0, 4, &
               'Switch for blanket thermodynamic model')

       case ('afwi')
          call parse_real_variable('afwi', afwi, 1.0D-3, 0.05D0, &
               'I/B fw/blkt coolant channel inner radius (m)')
       case ('afwo')
          call parse_real_variable('afwo', afwo, 1.0D-3, 0.05D0, &
               'O/B fw/blkt coolant channel inner radius (m)')
       case ('inlet_temp')
          call parse_real_variable('inlet_temp', inlet_temp, 200.0D0, 600.0D0, &
               'Coolant inlet temperature (K)')
       case ('irefprop')
          call parse_int_variable('irefprop', irefprop, 0, 1, &
               'Switch to use REFPROP routines')
       case ('outlet_temp')
          call parse_real_variable('outlet_temp', outlet_temp, 450.0D0, 900.0D0, &
               'Coolant outlet temperature (K)')
       case ('nblktmodpo')
          call parse_int_variable('nblktmodpo', nblktmodpo, 1, 16, &
               'No of o/b blanket modules in poloidal direction')
       case ('nblktmodpi')
          call parse_int_variable('nblktmodpi', nblktmodpi, 1, 16, &
               'No of i/b blanket modules in poloidal direction')
       case ('nblktmodto')
          call parse_int_variable('nblktmodto', nblktmodto, 8, 96, &
               'No of o/b blanket modules in toroidal direction')
       case ('nblktmodti')
          call parse_int_variable('nblktmodti', nblktmodti, 8, 96, &
               'No of i/b blanket modules in toroidal direction')
       case ('tfwmatmax')
          call parse_real_variable('tfwmatmax', tfwmatmax, 500.0D0, 2000.0D0, &
               'Max temperature of first wall material (K)')
       case ('fw_th_conductivity')
          call parse_real_variable('fw_th_conductivity', fw_th_conductivity, 1.0D0, 100.0D0, &
               'thermal conductivity of first wall material at 293 K (W/m/K)')

       case ('etaiso')
          call parse_real_variable('etaiso', etaiso, 0.1D0, 1.0D0, &
               'Isentropic efficiency of coolant pumps')
       case ('blkttype')
          call parse_int_variable('blkttype', blkttype, 1, 3, &
               'Switch for blanket type')
       case ('blktmodel')
          call parse_int_variable('blktmodel', blktmodel, 0, 1, &
               'Switch for blanket neutronics calculations')
       case ('breedmat')
          call parse_int_variable('breedmat', breedmat, 1, 3, &
               'Switch for blanket breeder material')
       case ('declblkt')
          call parse_real_variable('declblkt', declblkt, 0.01D0, 0.2D0, &
               'Neutron decay length in blanket')
       case ('declfw')
          call parse_real_variable('declfw', declfw, 0.01D0, 0.2D0, &
               'Neutron decay length in first wall')
       case ('declshld')
          call parse_real_variable('declshld', declshld, 0.01D0, 0.2D0, &
               'Neutron decay length in blanket')
       case ('denstl')
          call parse_real_variable('denstl', denstl, 5.0D3, 1.0D4, &
               'Density of steel (kg/m3)')
       case ('denw')
          call parse_real_variable('denw', denw, 1.0D4, 5.0D4, &
               'Density of tungsten (kg/m3)')
       case ('emult')
          call parse_real_variable('emult', emult, 1.0D0, 2.0D0, &
               'Energy multip. in blanket and shield')
       case ('fblbe')
          call parse_real_variable('fblbe', fblbe, 0.0D0, 1.0D0, &
               'Beryllium fraction of blanket')
       case ('fblbreed')
          call parse_real_variable('fblbreed', fblbreed, 0.0D0, 1.0D0, &
               'Breeder fraction of blanket breeding unit')
       case ('fblhebmi')
          call parse_real_variable('fblhebmi', fblhebmi, 0.0D0, 1.0D0, &
               'Helium fraction of IB blanket box manifold')
       case ('fblhebmo')
          call parse_real_variable('fblhebmo', fblhebmo, 0.0D0, 1.0D0, &
               'Helium fraction of OB blanket box manifold')
       case ('fblhebpi')
          call parse_real_variable('fblhebpi', fblhebpi, 0.0D0, 1.0D0, &
               'Helium fraction of IB blanket back plate')
       case ('fblhebpo')
          call parse_real_variable('fblhebpo', fblhebpo, 0.0D0, 1.0D0, &
               'Helium fraction of OB blanket back plate')
       case ('fblli')
          call parse_real_variable('fblli', fblli, 0.0D0, 1.0D0, &
               'Lithium fraction of blanket')
       case ('fblli2o')
          call parse_real_variable('fblli2o', fblli2o, 0.0D0, 1.0D0, &
               'Li2O fraction of blanket')
       case ('fbllipb')
          call parse_real_variable('fbllipb', fbllipb, 0.0D0, 1.0D0, &
               'Li-Pb fraction of blanket')
       case ('fblss')
          call parse_real_variable('fblss', fblss, 0.0D0, 1.0D0, &
               'Stainless steel fraction of blanket')
       case ('fblvd')
          call parse_real_variable('fblvd', fblvd, 0.0D0, 1.0D0, &
               'Vanadium fraction of blanket')
       case ('fdiv')
          call parse_real_variable('fdiv', fdiv, 0.0D0, 1.0D0, &
               'Divertor area fraction')
       case ('fhcd')
          call parse_real_variable('fhcd', fhcd, 0.0D0, 1.0D0, &
               'HCD + diagnostics area fraction')
       case ('fhole')
          call parse_real_variable('fhole', fhole, 0.0D0, 1.0D0, &
               'Hole area fraction')
       case ('fvolbi')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'FVOLBI is now obsolete - (use FHOLE)'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('fvolbo')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'FVOLBO is now obsolete - (use FHOLE)'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('fvolcry')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'FVOLCRY is now obsolete -'
          write(outfile,*) 'please remove it from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '
          obsolete_var = .true.
       case ('fvoldw')
          call parse_real_variable('fvoldw', fvoldw, 0.0D0, 10.0D0, &
               'Fudge factor for vacuum vessel volume')
!+PJK FVOLSI, FVOLSO should now be restricted to <= 1
       case ('fvolsi')
          call parse_real_variable('fvolsi', fvolsi, 0.0D0, 10.0D0, &
               'Fudge factor for inboard shield volume')
       case ('fvolso')
          call parse_real_variable('fvolso', fvolso, 0.0D0, 10.0D0, &
               'Fudge factor for outboard shield volume')
!-PJK
       case ('fwclfr')
          call parse_real_variable('fwclfr', fwclfr, 0.0D0, 1.0D0, &
               'First wall coolant fraction')
       case ('fwbsshape')
          call parse_int_variable('fwbsshape', fwbsshape, 1, 2, &
               'Switch for fw/blanket/shield/vv shape')
       case ('fw_armour_thickness')
          call parse_real_variable('fw_armour_thickness', fw_armour_thickness, 0.0D0, 1.0D0, &
               'First wall W coating thickness (m)')
       case ('hcdportsize')
          call parse_int_variable('hcdportsize', hcdportsize, 1, 2, &
               'H/CD port size')
       case ('iblanket')
          call parse_int_variable('iblanket', iblanket, 1, 4, 'Switch for blanket model')
          if (iblanket == 3) then
              fwith = 0.03D0
              fwoth = 0.03D0
              fw_armour_thickness = 0.003D0
          end if
       case ('iblnkith')
          call parse_int_variable('iblnkith', iblnkith, 0, 1, 'Switch for inboard blanket')

       case ('inuclear')
         call parse_int_variable('inuclear', inuclear, 0, 1, &
              'switch for nuclear heating in the coils')
       case ('qnuc')
          call parse_real_variable('qnuc', qnuc, 0.0D0, 1.0D6, &
               'nuclear heating in the coils (W)')

       case ('li6enrich')
          call parse_real_variable('li6enrich', li6enrich, 7.40D0, 100.0D0, &
               'Li-6 enrichment')
       
       ! CCFE hcpb BB module (also includes the CP shielding for ST)
       case ('breeder_f')
          call parse_real_variable('breeder_f', breeder_f, 0.00D0, 1.0D0, &
               'Volume of Li4SiO4 / (Volume of Be12Ti + Li4SiO4)')
       case ('breeder_multiplier')
          call parse_real_variable('breeder_multiplier', breeder_multiplier, 0.0D0, 1.0D0, &
               'combined breeder/multipler fraction of blanket by volume')
       case ('vfcblkt')
          call parse_real_variable('vfcblkt', vfcblkt, 0.0D0, 1.0D0, &
               'He coolant fraction of blanket by volume')
       case ('vfpblkt')
          call parse_real_variable('vfpblkt', vfpblkt, 0.0D0, 1.0D0, &
               'He purge gas fraction of blanket by volume')
       case ('f_neut_shield')
         call parse_real_variable('f_neut_shield', f_neut_shield, 0.0D0, 1.0D0, &
              'He purge gas fraction of blanket by volume')

       case ('iblanket_thickness')
          call parse_int_variable('iblanket_thickness', iblanket_thickness, 1, 3, &
               'Blanket thickness switch')
          if (iblanket_thickness == 1) then
            blnkith = 0.53D0
            blnkoth = 0.91D0
          else if (iblanket_thickness == 2) then
            blnkith = 0.64D0
            blnkoth = 1.11D0
          else if (iblanket_thickness == 3) then
            blnkith = 0.75D0
            blnkoth = 1.30D0
          end if
       case ('npdiv')
          call parse_int_variable('npdiv', npdiv, 0, 4, &
               'Number of divertor ports')
       case ('nphcdin')
          call parse_int_variable('nphcdin', nphcdin, 0, 4, &
               'Number of inboard H/CD ports')
       case ('nphcdout')
          call parse_int_variable('nphcdout', nphcdout, 0, 4, &
               'Number of outboard H/CD ports')
       case ('vfblkt')
          call parse_real_variable('vfblkt', vfblkt, 0.0D0, 1.0D0, &
               'Coolant void fraction in blanket')
       case ('vfshld')
          call parse_real_variable('vfshld', vfshld, 0.0D0, 1.0D0, &
               'Coolant void fraction in shield')
       case ('wallpf')
          call parse_real_variable('wallpf', wallpf, 1.0D0, 2.0D0, &
               'Neutron wall load peaking factor')

          !  Heat transport / power settings

       case ('baseel')
          call parse_real_variable('baseel', baseel, 1.0D6, 1.0D10, &
               'Base plant electric load (W)')
       case ('etahtp')
          call parse_real_variable('etahtp', etahtp, 0.1D0, 1.0D0, &
               'Coolant pump electrical efficiency')
        case ('etatf')
          call parse_real_variable('etatf', etatf, 0.0D0, 1.0D0, &
               'AC to resistive power conversion for TF coils')
       case ('etath')
          call parse_real_variable('etath', etath, 0.0D0, 1.0D0, &
               'Thermal-electric conversion efficiency')
       case ('fmgdmw')
          call parse_real_variable('fmgdmw', fmgdmw, 0.0D0, 100.0D0, &
               'Power to MGF units (MW)')
       case ('fpumpblkt')
          call parse_real_variable('fpumpblkt', fpumpblkt, 0.0D0, 0.2D0, &
               'Blanket pumping/thermal power ratio')
       case ('fpumpdiv')
          call parse_real_variable('fpumpdiv', fpumpdiv, 0.0D0, 0.2D0, &
               'Divertor pumping/thermal power ratio')
       case ('fpumpfw')
          call parse_real_variable('fpumpfw', fpumpfw, 0.0D0, 0.2D0, &
               'First wall pumping/thermal power ratio')
       case ('fpumpshld')
          call parse_real_variable('fpumpshld', fpumpshld, 0.0D0, 0.2D0, &
               'Shield pumping/thermal power ratio')
       case ('htpmw_min')
          call parse_real_variable('htpmw_min', htpmw_min, 0.0D0, 500.0D0, &
               'Minimum total electrical power for primary coolant pumps  (MW)')
       case ('ipowerflow')
          call parse_int_variable('ipowerflow', ipowerflow, 0, 1, &
               'Switch for power flow model')
       case ('iprimnloss')
          call parse_int_variable('iprimnloss', iprimnloss, 0, 1, &
               'Switch for lost neutron power destiny')
       case ('iprimshld')
          call parse_int_variable('iprimshld', iprimshld, 0, 1, &
               'Switch for shield thermal power destiny')
       case ('pwpm2')
          call parse_real_variable('pwpm2', pwpm2, 0.0D0, 1.0D3, &
               'Base AC power requirement (W/m2)')
       case ('pinjmax')
          call parse_real_variable('pinjmax', pinjmax, 0.0D0, 1.0D3, &
               'Maximum injector wall plug power during pulse (MW)')
       case ('trithtmw')
          call parse_real_variable('trithtmw', trithtmw, 0.0D0, 100.0D0, &
               'Tritium process power (MW)')
       case ('vachtmw')
          call parse_real_variable('vachtmw', vachtmw, 0.0D0, 100.0D0, &
               'Vacuum pump power (MW)')

          !  Cost information settings

       case ('abktflnc')
          call parse_real_variable('abktflnc', abktflnc, 0.1D0, 100.0D0, &
               'Allowable blanket fluence (MW-yr/m2)')
       case ('adivflnc')
          call parse_real_variable('adivflnc', adivflnc, 0.1D0, 100.0D0, &
               'Allowable divertor fluence (MW-yr/m2)')
       case ('cfactr')
          call parse_real_variable('cfactr', cfactr, 0.0D0, 1.0D0, &
               'Plant capacity factor or availability')
       case ('cfind')
          call parse_real_array('cfind', cfind, isub1, 4, &
               'Indirect cost factor vs LSA', icode)
       case ('cpstflnc')
          call parse_real_variable('cpstflnc', cpstflnc, 0.01D0, 30.0D0, &
               'Allowable centrepost neutron fluence (MW-yr/m2)')
       case ('decomf')
          call parse_real_variable('decomf', decomf, 0.0D0, 1.0D0, &
               'Decommissioning fund fraction')
       case ('dintrt')
          call parse_real_variable('dintrt', dintrt, 0.0D0, 0.1D0, &
               'Borrowing - saving interest rate difference')
       case ('dtlife')
          call parse_real_variable('dtlife', dtlife, 0.0D0, 15.0D0, &
               'Decommissioning time prior to end of plant')
       case ('fcap0')
          call parse_real_variable('fcap0', fcap0, 1.0D0, 1.5D0, &
               'Ave cost of money for plant construction')
       case ('fcap0cp')
          call parse_real_variable('fcap0cp', fcap0cp, 1.0D0, 1.5D0, &
               'Ave cost of money for replaceable components')
       case ('fcdfuel')
          call parse_real_variable('fcdfuel', fcdfuel, 0.0D0, 1.0D0, &
               'Fraction of CD cost assumed fuel cost')
       case ('fcr0')
          call parse_real_variable('fcr0', fcr0, 0.0D0, 1.0D0, &
               'Fixed charge rate during construction')
       case ('fkind')
          call parse_real_variable('fkind', fkind, 0.5D0, 1.0D0, &
               'Multiplier for Nth of a kind costs')
       case ('ifueltyp')
          call parse_int_variable('ifueltyp', ifueltyp, 0, 2, &
               'Switch for costing of 1st wall etc.')
       case ('ipnet')
          call parse_int_variable('ipnet', ipnet, 0, 1, &
               'Switch for net electric power calc.')
       case ('ireactor')
          call parse_int_variable('ireactor', ireactor, 0, 1, &
               'Switch for MWe / C-o-E calculation')
       case ('lsa')
          call parse_int_variable('lsa', lsa, 1, 4, &
               'Level of safety assurance')
       case ('output_costs')
          call parse_int_variable('output_costs', output_costs, 0, 1, &
               'Switch for writing costs to file')
       case ('ratecdol')
          call parse_real_variable('ratecdol', ratecdol, 0.0D0, 0.5D0, &
               'Effective cost of money')

          !  Unit cost settings

       case ('cconfix')
          call parse_real_variable('cconfix', cconfix, 50.0D0, 200.0D0, &
               'Fixed cost of superconducting cable ($/m)')
       case ('cconshpf')
          call parse_real_variable('cconshpf', cconshpf, 50.0D0, 200.0D0, &
               'PF coil steel conduit/sheath cost ($/m)')
       case ('cconshtf')
          call parse_real_variable('cconshtf', cconshtf, 50.0D0, 200.0D0, &
               'TF coil steel conduit/sheath cost ($/m)')
       case ('cland')
          call parse_real_variable('cland', cland, 10.0D0, 100.0D0, &
               'Cost of land (M$)')
       case ('costexp')
          call parse_real_variable('costexp', costexp, 0.01D0, 5.0D0, &
               'Cost exponent for 2015 costs model')
       case ('costexp_pebbles')
          call parse_real_variable('costexp_pebbles', costexp_pebbles, 0.01D0, 5.0D0, &
               'cost exponent for pebbles in 2015 costs model')
       case ('cost_factor_buildings')
          call parse_real_variable('cost_factor_buildings', cost_factor_buildings, 0.1D0, 10.0D0, &
           'Cost scaling factor for buildings (2015 costs model)')
       case ('cost_factor_land')
          call parse_real_variable('cost_factor_land', cost_factor_land, 0.1D0, 10.0D0, &
          'Cost scaling factor for land (2015 costs model)')
       case ('cost_factor_tf_coils')
          call parse_real_variable('cost_factor_tf_coils', cost_factor_tf_coils, 0.1D0, 10.0D0, &
          'Cost scaling factor for TF coils (2015 costs model)')
       case ('cost_factor_fwbs')
          call parse_real_variable('cost_factor_fwbs', cost_factor_fwbs, 0.1D0, 10.0D0, &
          'Cost scaling factor for fwbs (2015 costs model)')
       case ('cost_factor_rh')
          call parse_real_variable('cost_factor_rh', cost_factor_rh, 0.1D0, 10.0D0, &
          'Cost scaling factor for remote handling (2015 costs model)')
       case ('cost_factor_vv')
          call parse_real_variable('cost_factor_vv', cost_factor_vv, 0.1D0, 10.0D0, &
          'Cost scaling factor for vacuum vessel (2015 costs model)')
       case ('cost_factor_bop')
          call parse_real_variable('cost_factor_bop', cost_factor_bop, 0.1D0, 10.0D0, &
          'Cost scaling factor for energy conversion system (2015 costs model)')
       case ('cost_factor_misc')
          call parse_real_variable('cost_factor_misc', cost_factor_misc, 0.1D0, 10.0D0, &
          'Cost scaling factor for remaining subsystems (2015 costs model)')
       case ('maintenance_fwbs')
          call parse_real_variable('maintenance_fwbs', maintenance_fwbs, 0.0D0, 1.0D0, &
          'Maintenance cost factor: first wall, blanket, shield, divertor')
       case ('maintenance_gen')
          call parse_real_variable('maintenance_gen', maintenance_gen, 0.0D0, 1.0D0, &
          'Maintenance cost factor: All other components except coils, vacuum vessel, thermal shield, cryostat, land')
       case ('amortization')
          call parse_real_variable('amortization', amortization, 1.0D0, 50.0D0, &
               'amortization factor (fixed charge factor) "A" (years)')

       case ('cost_model')
          call parse_int_variable('cost_model', cost_model, 0, 2, &
               'Switch for cost model')
       case ('cowner')
          call parse_real_variable('cowner', cowner, 0.0D0, 1.0D0, &
               'Owner cost factor')
       case ('csi')
          call parse_real_variable('csi', csi, 1.0D0, 100.0D0, &
               'Allowance for site costs (M$)')
       case ('cturbb')
          call parse_real_variable('cturbb', cturbb, 100.0D0, 1000.0D0, &
               'Cost of turbine building (M$)')
       case ('fcontng')
          call parse_real_variable('fcontng', fcontng, 0.0D0, 1.0D0, &
               'Project contingency factor')
       case ('step_ref')
          call parse_real_array('step_ref', step_ref, isub1, 68, &
               'Reference values for cost model 2', icode)
       case ('ucblbe')
          call parse_real_variable('ucblbe', ucblbe, 1.0D0, 1.0D3, &
               'Unit cost for blanket Be ($/kg)')
       case ('ucblbreed')
          call parse_real_variable('ucblbreed', ucblbreed, 1.0D0, 1.0D3, &
               'Unit cost for blanket breeder material ($/kg)')
       case ('ucblli')
          call parse_real_variable('ucblli', ucblli, 1.0D1, 1.0D4, &
               'Unit cost for blanket Li ($/kg)')
       case ('ucblli2o')
          call parse_real_variable('ucblli2o', ucblli2o, 1.0D2, 1.0D4, &
               'Unit cost for blanket Li2O ($/kg)')
       case ('ucbllipb')
          call parse_real_variable('ucbllipb', ucbllipb, 1.0D2, 1.0D4, &
               'Unit cost for blanket Li-Pb ($/kg)')
       case ('ucblss')
          call parse_real_variable('ucblss', ucblss, 10.0D0, 1.0D3, &
               'Unit cost for blanket st.steel ($/kg)')
       case ('ucblvd')
          call parse_real_variable('ucblvd', ucblvd, 100.0D0, 1.0D3, &
               'Unit cost for blanket Vd ($/kg)')
       case ('ucbus')
          call parse_real_variable('ucbus', ucbus, 0.01D0, 10.0D0, &
               'Cost of Al bus for TF coil ($/A-m)')
       case ('uccase')
          call parse_real_variable('uccase', uccase, 1.0D0, 1.0D3, &
               'Cost of superconductor case ($/kg)')
       case ('uccpcl1')
          call parse_real_variable('uccpcl1', uccpcl1, 1.0D0, 1.0D3, &
               'Cost of tapered copper ($/kg)')
       case ('uccpclb')
          call parse_real_variable('uccpclb', uccpclb, 1.0D0, 1.0D3, &
               'Cost TF outer leg plate coils ($/kg)')
       case ('uccry')
          call parse_real_variable('uccry', uccry, 1.0D4, 1.0D6, &
               'Heat transport cryoplant costs ($/W)')
       case ('uccryo')
          call parse_real_variable('uccryo', uccryo, 1.0D0, 1.0D3, &
               'Unit cost for cryostat ($/kg)')
       case ('uccu')
          call parse_real_variable('uccu', uccu, 10.0D0, 1.0D2, &
               'Copper in SC cable cost ($/kg)')
       case ('ucdiv')
          call parse_real_variable('ucdiv', ucdiv, 1.0D3, 1.0D7, &
               'Cost of divertor blade ($)')
       case ('ucech')
          call parse_real_variable('ucech', ucech, 1.0D0, 10.0D0, &
               'ECH system cost ($/W)')
       case ('ucf1')
          call parse_real_variable('ucf1', ucf1, 1.0D6, 50.0D6, &
               'Fuelling system cost ($)')
       case ('ucfnc')
          call parse_real_variable('ucfnc', ucfnc, 10.0D0, 100.0D0, &
               'Outer PF fence support cost ($/kg)')
       case ('ucfuel')
          call parse_real_variable('ucfuel', ucfuel, 1.0D0, 10.0D0, &
               'Cost of fuel (M$/yr)')
       case ('uche3')
          call parse_real_variable('uche3', uche3, 1.0D5, 1.0D7, &
               'Cost of He3 fuel ($/kg)')
       case ('uchrs')
          call parse_real_variable('uchrs', uchrs, 1.0D7, 5.0D8, &
               'Cost of heat rejection system ($)')
       case ('uchts')
          call parse_real_array('uchts', uchts, isub1, 2, &
               'Cost of heat transp system equip per loop ($/W)', icode)
       case ('uciac')
          call parse_real_variable('uciac', uciac, 1.0D7, 1.0D9, &
               'Cost of instrum, control & diag.($)')
       case ('ucich')
          call parse_real_variable('ucich', ucich, 1.0D0, 10.0D0, &
               'Cost of ICH system ($/W)')
       case ('uclh')
          call parse_real_variable('uclh', uclh, 1.0D0, 10.0D0, &
               'LH system cost ($/W)')
       case ('ucme')
          call parse_real_variable('ucme', ucme, 1.0D7, 1.0D9, &
               'cost of maintenance equip. ($)')
       case ('ucmisc')
          call parse_real_variable('ucmisc', ucmisc, 1.0D7, 5.0D7, &
               'Miscellaneous plant allowance ($)')
       case ('ucnbi')
          call parse_real_variable('ucnbi', ucnbi, 1.0D0, 10.0D0, &
               'NBI system cost ($/W)')
       case ('ucoam')
          call parse_real_array('ucoam', ucoam, isub1, 4, &
               'Annual cost of operation and maintenance', icode)
       case ('ucpens')
          call parse_real_variable('ucpens', ucpens, 1.0D0, 100.0D0, &
               'Penetration shield cost ($/kg)')
       case ('ucpfb')
          call parse_real_variable('ucpfb', ucpfb, 1.0D0, 1.0D3, &
               'Cost of PF coil buses ($/kA-m)')
       case ('ucpfbk')
          call parse_real_variable('ucpfbk', ucpfbk, 1.0D3, 1.0D5, &
               'Cost of PF coil DC breakers ($/MVA)')
       case ('ucpfbs')
          call parse_real_variable('ucpfbs', ucpfbs, 1.0D3, 1.0D4, &
               'Cost of PF burn power supplies ($/kW**0.7)')
       case ('ucpfcb')
          call parse_real_variable('ucpfcb', ucpfcb, 1.0D3, 1.0D5, &
               'Cost of PF coil AC breakers ($/circ)')
       case ('ucpfdr1')
          call parse_real_variable('ucpfdr1', ucpfdr1, 1.0D0, 1.0D3, &
               'Cost factor for dump resistors ($/MJ)')
       case ('ucpfic')
          call parse_real_variable('ucpfic', ucpfic, 1.0D3, 1.0D5, &
               'Cost of PF instrum & cont ($/channel)')
       case ('ucpfps')
          call parse_real_variable('ucpfps', ucpfps, 1.0D3, 1.0D5, &
               'Cost of PF coil pulsed P.S. ($/MVA)')
       case ('ucrb')
          call parse_real_variable('ucrb', ucrb, 1.0D2, 1.0D3, &
               'Cost of reactor building ($/m3)')
       case ('ucsc')
          call parse_real_array('ucsc', ucsc, isub1, 5, &
               'Cost of superconductor ($/kg)', icode)
       case ('ucshld')
          call parse_real_variable('ucshld', ucshld, 1.0D0, 100.0D0, &
               'Cost of shield structural steel ($/kg)')
       case ('uctfbr')
          call parse_real_variable('uctfbr', uctfbr, 1.0D0, 10.0D0, &
               'Cost of TF coil breakers ($/W**0.7)')
       case ('uctfbus')
          call parse_real_variable('uctfbus', uctfbus, 1.0D0, 1.0D3, &
               'Cost of TF coil bus ($/kg)')
       case ('uctfps')
          call parse_real_variable('uctfps', uctfps, 1.0D0, 1.0D3, &
               'Cost of TF power supplies ($/W**0.7)')
       case ('uctfsw')
          call parse_real_variable('uctfsw', uctfsw, 0.1D0, 10.0D0, &
               'Cost of TF slow dump switches ($/A)')
       case ('ucturb')
          call parse_real_array('ucturb', ucturb, isub1, 2, &
               'Cost of turbine plant equipment ($)', icode)
       case ('ucwindpf')
          call parse_real_variable('ucwindpf', ucwindpf, 100.0D0, 1.0D3, &
               'Cost of SCPF windings ($/m)')
       case ('ucwindtf')
          call parse_real_variable('ucwindtf', ucwindtf, 100.0D0, 1.0D3, &
               'Cost of SCTF windings ($/m)')
       case ('ucwst')
          call parse_real_array('ucwst', ucwst, isub1, 4, &
               'cost of waste disposal (M$/yr)', icode)

          !  Availability settings

       case ('iavail')
          call parse_int_variable('iavail', iavail, 0, 2, &
               'Switch for plant availability model')
       case ('avail_min')
          call parse_real_variable('avail_min', avail_min, 0.0D0, 1.0D0, &
               'Required minimum availability (constraint equation 61)')
       case ('favail')
          call parse_real_variable('favail', favail, 0.0D0, 1.0D0, &
               'F-value for minimum availability (constraint equation 61)')
       case ('num_rh_systems')
          call parse_int_variable('num_rh_systems', num_rh_systems, 1, 10, &
               'Number of remote handling systems (from 1-10)')
       case ('conf_mag')
          call parse_real_variable('conf_mag', conf_mag, 0.9D0, 1.0D0, &
               'c parameter, which determines the temperature margin at which magnet lifetime starts to decline')
       case ('div_prob_fail')
          call parse_real_variable('div_prob_fail', div_prob_fail, 0.0D0, 1.0D0, &
               'Divertor probability of failure (per op day)')
       case ('div_umain_time')
          call parse_real_variable('div_umain_time', div_umain_time, 0.1D0, 2.0D0, &
               'Divertor unplanned maintenance time (years)')

       case ('div_nref')
          call parse_real_variable('div_nref', div_nref, 1.0D3, 1.0D8, &
               'Reference value for cycle life of divertor')
       case ('div_nu')
          call parse_real_variable('div_nu', div_nu, 1.0D3, 1.0D8, &
               'The cycle when the divertor fails with 100% probability')

       case ('fwbs_nref')
          call parse_real_variable('fwbs_nref', fwbs_nref, 1.0D3, 1.0D8, &
               'Reference value for cycle life of blanket')
       case ('fwbs_nu')
          call parse_real_variable('fwbs_nu', fwbs_nu, 1.0D3, 1.0D8, &
               'The cycle when the blanket fails with 100% probability')

       case ('fwbs_prob_fail')
          call parse_real_variable('fwbs_prob_fail', fwbs_prob_fail, 0.0D0, 1.0D0, &
               'Fwbs probability of failure (per op day)')
       case ('fwbs_umain_time')
          call parse_real_variable('fwbs_umain_time', fwbs_umain_time, 0.1D0, 2.0D0, &
               'Fwbs unplanned maintenace time (years)')
       case ('redun_vacp')
          call parse_real_variable('redun_vacp', redun_vacp, 0.0D0, 100.0D0, &
               'Vacuum system pump redundancy level (%)')
       case ('tbktrepl')
          call parse_real_variable('tbktrepl', tbktrepl, 0.01D0, 2.0D0, &
               'Time needed to replace blanket (yr)')
       case ('tcomrepl')
          call parse_real_variable('tcomrepl', tcomrepl, 0.01D0, 2.0D0, &
               'Time needed to replace blanket+divertor (yr)')
       case ('tdivrepl')
          call parse_real_variable('tdivrepl', tdivrepl, 0.01D0, 2.0D0, &
               'Time needed to replace divertor (yr)')
       case ('tlife')
          call parse_real_variable('tlife', tlife, 1.0D0, 100.0D0, &
               'Plant life (yr)')
       case ('uubop')
          call parse_real_variable('uubop', uubop, 0.005D0, 0.1D0, &
               'Unplanned unavailability for BOP')
       case ('uucd')
          call parse_real_variable('uucd', uucd, 0.005D0, 0.1D0, &
               'Unplanned unavailability for CD system')
       case ('uudiv')
          call parse_real_variable('uudiv', uudiv, 0.005D0, 0.1D0, &
               'Unplanned unavailability for divertor')
       case ('uufuel')
          call parse_real_variable('uufuel', uufuel, 0.005D0, 0.1D0, &
               'Unplanned unavailability for fuel system')
       case ('uufw')
          call parse_real_variable('uufw', uufw, 0.005D0, 0.1D0, &
               'Unplanned unavailability for first wall')
       case ('uumag')
          call parse_real_variable('uumag', uumag, 0.005D0, 0.1D0, &
               'Unplanned unavailability for magnets')
       case ('uuves')
          call parse_real_variable('uuves', uuves, 0.005D0, 0.1D0, &
               'Unplanned unavailability for vessel')


        !  Sweep settings

       case ('isweep')
          call parse_int_variable('isweep', isweep, 0, ipnscns, &
               'Number of scans to perform')
       case ('nsweep')
          call parse_int_variable('nsweep', nsweep, 1, ipnscnv, &
               'Variable used in scan')
       case ('sweep')
          call parse_real_array('sweep', sweep, isub1, ipnscns, &
               'Actual values to use in scan', icode)

        case ('scan_dim')
          call parse_int_variable('scan_dim', scan_dim, 1, 2, &
               'Switch for 1-D or 2-D scan')
        case ('isweep_2')
          call parse_int_variable('isweep_2', isweep_2, 0, ipnscns, &
               'Number of 2D scans to perform')
        case ('nsweep_2')
          call parse_int_variable('nsweep_2', nsweep_2, 1, ipnscnv, &
               'Second variable used in 2D scan')
        case ('sweep_2')
          call parse_real_array('sweep_2', sweep_2, isub1, ipnscns, &
               'Actual values to use in 2D scan', icode)

          !  Buildings settings

       case ('admv')
          call parse_real_variable('admv', admv, 1.0D4, 1.0D6, &
               'Administration building volume (m3)')
       case ('clh1')
          call parse_real_variable('clh1', clh1, 0.0D0, 20.0D0, &
               'Clearance TF coil to cryostat top (m)')
       case ('clh2')
          call parse_real_variable('clh2', clh2, 0.0D0, 30.0D0, &
               'Clearance TF coil to foundation (m)')
       case ('conv')
          call parse_real_variable('conv', conv, 1.0D4, 1.0D6, &
               'Control building volume (m3)')
       case ('esbldgm3')
          call parse_real_variable('esbldgm3', esbldgm3, 1.0D3, 1.0D6, &
               'Energy storage building volume (m3)')
       case ('fndt')
          call parse_real_variable('fndt', fndt, 0.0D0, 10.0D0, &
               'Foundation thickness (m)')
       case ('hccl')
          call parse_real_variable('hccl', hccl, 0.0D0, 10.0D0, &
               'Clearance around components in hot cell (m)')
       case ('hcwt')
          call parse_real_variable('hcwt', hcwt, 0.0D0, 10.0D0, &
               'Hot cell wall thickness (m)')
       case ('mbvfac')
          call parse_real_variable('mbvfac', mbvfac, 0.9D0, 3.0D0, &
               'Maintenance building volume multiplier')
       case ('pfbldgm3')
          call parse_real_variable('pfbldgm3', pfbldgm3, 1.0D4, 1.0D6, &
               'PF coil power conv. bldg volume (m3)')
       case ('pibv')
          call parse_real_variable('pibv', pibv, 1.0D3, 1.0D5, &
               'Power injection building volume (m3)')
       case ('rbrt')
          call parse_real_variable('rbrt', rbrt, 0.0D0, 10.0D0, &
               'Reactor building roof thickness (m)')
       case ('rbvfac')
          call parse_real_variable('rbvfac', rbvfac, 0.9D0, 3.0D0, &
               'Reactor building volume multiplier')
       case ('rbwt')
          call parse_real_variable('rbwt', rbwt, 0.0D0, 10.0D0, &
               'Reactor building wall thickness (m)')
       case ('row')
          call parse_real_variable('row', row, 0.0D0, 10.0D0, &
               'Wall clearance for cranes (m)')
       case ('rxcl')
          call parse_real_variable('rxcl', rxcl, 0.0D0, 10.0D0, &
               'Clearance around reactor (m)')
       case ('shmf')
          call parse_real_variable('shmf', shmf, 0.0D0, 1.0D0, &
               'Fraction of TF shield mass per lift')
       case ('shov')
          call parse_real_variable('shov', shov, 1.0D3, 1.0D6, &
               'Shops and warehouse volume (m3)')
       case ('stcl')
          call parse_real_variable('stcl', stcl, 0.0D0, 10.0D0, &
               'Clearance above crane to roof (m)')
       case ('tfcbv')
          call parse_real_variable('tfcbv', tfcbv, 1.0D4, 1.0D6, &
               'TF coil power conv. bldg volume (m3)')
       case ('trcl')
          call parse_real_variable('trcl', trcl, 0.0D0, 10.0D0, &
               'Transport clearance between comps (m)')
       case ('triv')
          call parse_real_variable('triv', triv, 1.0D4, 1.0D6, &
               'Tritium building volume (m3)')
       case ('wgt')
          call parse_real_variable('wgt', wgt, 1.0D4, 1.0D6, &
               'Reactor building crane capacity (kg)')
       case ('wgt2')
          call parse_real_variable('wgt2', wgt2, 1.0D4, 1.0D6, &
               'Hot cell crane capacity (kg)')
       case ('wsvfac')
          call parse_real_variable('wsvfac', wsvfac, 0.9D0, 3.0D0, &
               'Warm shop building volume multiplier')

          !  Energy storage settings

       case ('iscenr')
          call parse_int_variable('iscenr', iscenr, 1, 3, &
               'Switch for energy storage option')

       case ('maxpoloidalpower')
          call parse_real_variable('maxpoloidalpower', maxpoloidalpower, 0.0D0, 2.0D3, &
               'Maximum permitted absolute rate of change of stored energy in poloidal field (MW)')

          !  Output file options settings

       case ('sect01', 'sect02', 'sect03', 'sect04', 'sect05', &
            'sect06', 'sect07', 'sect08', 'sect09', 'sect10', &
            'sect11', 'sect12', 'sect13', 'sect14', 'sect15', &
            'sect16', 'sect17', 'sect18', 'sect19', 'sect20', &
            'sect21')
          write(outfile,*) ' '
          write(outfile,*) '**********'
          write(outfile,*) 'SECT flags are now ignored -'
          write(outfile,*) 'please remove them from the input file'
          write(outfile,*) '**********'
          write(outfile,*) ' '

          !  Vacuum system settings

       case ('vacuum_model')
          call parse_string_variable('vacuum_model', vacuum_model, 'vacuum_model')


       case ('ntype')
          call parse_int_variable('ntype', ntype, 0, 1, &
               'Pump type')
       case ('pbase')
          call parse_real_variable('pbase', pbase, 1.0D-8, 1.0D-3, &
               'Base pressure (Pa)')
       case ('prdiv')
          call parse_real_variable('prdiv', prdiv, 0.0D0, 10.0D0, &
               'Divertor chamber pressure in burn (Pa)')
       case ('pumptp')
          call parse_real_variable('pumptp', pumptp, 0.0D0, 1.0D30, &
               'Pump throughput (molecules/s) (default is ITER value)')
       case ('rat')
          call parse_real_variable('rat', rat, 1.0D-10, 1.0D-6, &
               'Plas chamber wall outgas rate (Pa-m/s)')
       case ('tn')
          call parse_real_variable('tn', tn, 1.0D0, 1.0D3, &
               'Neutral gas temp in chamber (K)')
       case ('dwell_pump')
               call parse_int_variable('dwell_pump', dwell_pump, 0, 2, &
                    'switch for dwell pumping options')
       case ('pumpareafraction')
          call parse_real_variable('pumpareafraction', pumpareafraction, 1.0D-6, 1.0D0, &
               'Area of one pumping port as a fraction of plasma surface area')
      case ('pumpspeedmax')
          call parse_real_variable('pumpspeedmax', pumpspeedmax, 1.0D-6, 1.0D3, &
               'Maximum pumping speed per unit area for deuterium & tritium, molecular flow')
      case ('pumpspeedfactor')
          call parse_real_variable('pumpspeedfactor', pumpspeedfactor, 1.0D-6, 1.0D0, &
               'Effective pumping speed reduction factor due to duct impedance')
      case ('initialpressure')
          call parse_real_variable('initialpressure', initialpressure, 1.0D-6, 1.0D4, &
               'initial neutral pressure at the beginning of the dwell phase (Pa)')
      case ('outgasindex')
          call parse_real_variable('outgasindex', outgasindex, 1.0D-6, 1.0D3, &
               'outgassing decay index')
      case ('outgasfactor')
          call parse_real_variable('outgasfactor', outgasfactor, 1.0D-6, 1.0D3, &
               'outgassing prefactor kw: outgassing rate at 1 s per unit area (Pa m s-1)')

          ! Reinke criterion
       case ('lhat')
          call parse_real_variable('lhat', lhat, 1.0D0, 1.5D1, &
               'connection length factor')

       case ('reinke_mode')
          call parse_int_variable('reinke_mode', reinke_mode, 0, 1, &
               'Switch for Reinke Criterion mode (0=H, 1=I)')

       case ('impvardiv')
          call parse_int_variable('impvardiv', impvardiv, 3, nimp, &
               'Index of impurity to be iterated for Reike criterion')


          !  Stellarator settings

       case ('istell')
          call parse_int_variable('istell', istell, 0, 5, &
               'Stellarator machine specification (1=Helias5, 2=Helias4, 3=Helias3)')
       case ('bmn')
          call parse_real_variable('bmn', bmn, 1.0D-4, 1.0D-2, &
               'Relative radial field perturbation')
       case ('f_asym')
          call parse_real_variable('f_asym', f_asym, 0.9D0, 2.0D0, &
               'Heat load peaking factor')
       case ('f_rad')
          call parse_real_variable('f_rad', f_rad, 0.0D0, 1.0D0, &
               'Radiated power fraction in SOL')
       case ('f_w')
          call parse_real_variable('f_w', f_w, 0.1D0, 1.0D0, &
               'Island size fraction factor')
       case ('fdivwet')
          call parse_real_variable('fdivwet', fdivwet, 0.01D0, 1.0D0, &
               'Wetted area fraction of divertor plates')
       case ('flpitch')
          call parse_real_variable('flpitch', flpitch, 1.0D-4, 1.0D-2, &
               'Field line pitch (rad)')
       case ('iotabar')
          call parse_real_variable('iotabar', iotabar, 0.1D0, 10.0D0, &
               'Stellarator rotational transform')
       case ('isthtr')
          call parse_int_variable('isthtr', isthtr, 1, 3, &
               'Stellarator method of auxiliary heating')
       case ('m_res')
          call parse_int_variable('m_res', m_res, 1, 10, &
               'Poloidal resonance number')
       case ('n_res')
          call parse_int_variable('n_res', n_res, 3, 6, &
               'Toroidal resonance number')
       case ('shear')
          call parse_real_variable('shear', shear, 0.1D0, 10.0D0, &
               'Magnetic shear')

       !  Inertial Fusion Energy plant settings

       case ('bldr')
          call parse_real_variable('bldr', bldr, 0.0D0, 10.0D0, &
                    'IFE blanket radial thickness (m)')
       case ('bldrc')
          call parse_real_variable('bldrc', bldrc, 0.0D0, 10.0D0, &
                    'IFE curtain radial thickness (m)')
       case ('bldzl')
          call parse_real_variable('bldzl', bldzl, 0.0D0, 10.0D0, &
                    'IFE blanket bottom part thickness (m)')
       case ('bldzu')
          call parse_real_variable('bldzu', bldzu, 0.0D0, 10.0D0, &
                    'IFE blanket top part thickness (m)')
       case ('blmatf')  !  N.B. actually a 2-D array
          call parse_real_array('blmatf', blmatf, isub1, 3*(maxmat+1), &
                    'IFE blanket material fraction', icode)
       case ('cdriv0')
          call parse_real_variable('cdriv0', cdriv0, 50.0D0, 500.0D0, &
                    'IFE driver cost offset (M$)')
       case ('cdriv1')
          call parse_real_variable('cdriv1', cdriv1, 50.0D0, 500.0D0, &
                    'IFE driver cost offset (M$)')
       case ('cdriv2')
          call parse_real_variable('cdriv2', cdriv2, 50.0D0, 500.0D0, &
                    'IFE driver cost offset (M$)')
       case ('chdzl')
          call parse_real_variable('chdzl', chdzl, 0.0D0, 10.0D0, &
                    'IFE chamber bottom part thickness (m)')
       case ('chdzu')
          call parse_real_variable('chdzu', chdzu, 0.0D0, 10.0D0, &
                    'IFE chamber top part thickness (m)')
       case ('chmatf')
          call parse_real_array('chmatf', chmatf, isub1, maxmat+1, &
                    'IFE chamber material fraction', icode)
       case ('chrad')
          call parse_real_variable('chrad', chrad, 0.1D0, 20.0D0, &
                    'IFE chamber radial thickness (m)')
       case ('dcdrv0')
          call parse_real_variable('dcdrv0', dcdrv0, 0.0D0, 200.0D0, &
                    'IFE driver cost gradient (M$/MJ)')
       case ('dcdrv1')
          call parse_real_variable('dcdrv1', dcdrv1, 0.0D0, 200.0D0, &
                    'IFE driver cost gradient (M$/MJ)')
       case ('dcdrv2')
          call parse_real_variable('dcdrv2', dcdrv2, 0.0D0, 200.0D0, &
                    'IFE driver cost gradient (M$/MJ)')
       case ('drveff')
          call parse_real_variable('drveff', drveff, 0.01D0, 1.0D0, &
                    'IFE driver efficiency')
       case ('edrive')
          call parse_real_variable('edrive', edrive, 1.0D5, 50.0D8, &
                    'IFE driver energy (J)')
       case ('etali')
          call parse_real_variable('etali', etali, 0.0D0, 1.0D0, &
                    'IFE lithium pump wall plug efficiency')
       case ('etave')
          call parse_real_array('etave', etave, isub1, 10, &
                    'IFE driver efficiency vs driver energy', icode)
       case ('fauxbop')
          call parse_real_variable('fauxbop', fauxbop, 0.0D0, 1.0D0, &
                    'Frac. of gross electric power to BOP (IFE)')
       case ('fbreed')
          call parse_real_variable('fbreed', fbreed, 0.0D0, 0.999D0, &
                    'Fraction of breeder outside core')
       case ('fburn')
          call parse_real_variable('fburn', fburn, 0.01D0, 1.0D0, &
                    'IFE burn fraction')
       case ('flirad')
          call parse_real_variable('flirad', flirad, 0.0D0, 10.0D0, &
                    'Radius of FLiBe inlet (HYLIFE) (m)')
       case ('frrmax')
          call parse_real_variable('frrmax', frrmax, 1.0D-6, 1.0D0, &
                    'F-value for IFE repetition rate')
       case ('fwdr')
          call parse_real_variable('fwdr', fwdr, 0.0D0, 10.0D0, &
                    'IFE first wall radial thickness (m)')
       case ('fwdzl')
          call parse_real_variable('fwdzl', fwdzl, 0.0D0, 10.0D0, &
                    'IFE first wall bottom part thickness (m)')
       case ('fwdzu')
          call parse_real_variable('fwdzu', fwdzu, 0.0D0, 10.0D0, &
                    'IFE first wall top part thickness (m)')
       case ('fwmatf')  !  N.B. actually a 2-D array
          call parse_real_array('fwmatf', fwmatf, isub1, 3*(maxmat+1), &
                    'IFE first wall material fraction', icode)
       case ('gainve')
          call parse_real_array('gainve', gainve, isub1, 10, &
                    'IFE target gain vs driver energy', icode)
       case ('htpmw_ife')
          call parse_real_variable('htpmw_ife', htpmw_ife, 0.0D0, 1.0D3, &
                    'IFE heat transport system electrical pump power (MW)')          
       case ('ifedrv')
          call parse_int_variable('ifedrv', ifedrv, -1, 3, &
                    'IFE driver type')
       case ('ifetyp')
          call parse_int_variable('ifetyp', ifetyp, 0, 4, &
                    'IFE device build type')
       case ('mcdriv')
          call parse_real_variable('mcdriv', mcdriv, 0.1D0, 10.0D0, &
                    'IFE driver cost multiplier')
       case ('pdrive')
          call parse_real_variable('pdrive', pdrive, 1.0D6, 200.0D6, &
                    'IFE driver power to target (W)')
       case ('pfusife')
          call parse_real_variable('pfusife', pfusife, 0.0D0, 1.0D4, &
                    'IFE input fusion power (MW) (ifedrv=3 only)')
       case ('pifecr')
          call parse_real_variable('pifecr', pifecr, 0.0D0, 100.0D0, &
                    'IFE cryogenic power (MW)')
       case ('ptargf')
          call parse_real_variable('ptargf', ptargf, 0.1D0, 100.0D0, &
                    'IFE target factory power at 6Hz (MW)')
       case ('rrin')
          call parse_real_variable('rrin', rrin, 0.1D0, 50.0D0, &
                     'Input IFE repetition rate (Hz) (ifedrv=3 only)')
       case ('rrmax')
          call parse_real_variable('rrmax', rrmax, 1.0D0, 50.0D0, &
                    'Maximum IFE repetition rate (Hz)')
       case ('shdr')
          call parse_real_variable('shdr', shdr, 0.0D0, 10.0D0, &
                    'IFE shield radial thickness (m)')
       case ('shdzl')
          call parse_real_variable('shdzl', shdzl, 0.0D0, 10.0D0, &
                    'IFE shield bottom part thickness (m)')
       case ('shdzu')
          call parse_real_variable('shdzu', shdzu, 0.0D0, 10.0D0, &
                    'IFE shield top part thickness (m)')
       case ('shmatf')  !  N.B. actually a 2-D array
          call parse_real_array('shmatf', shmatf, isub1, 3*(maxmat+1), &
                    'IFE shield material fraction', icode)
       case ('sombdr')
          call parse_real_variable('sombdr', sombdr, 0.0D0, 10.0D0, &
                    'Radius of SOMBRERO blanket bottom (m)')
       case ('somtdr')
          call parse_real_variable('somtdr', somtdr, 0.0D0, 10.0D0, &
                    'Radius of SOMBRERO blanket top (m)')
       case ('tgain')
          call parse_real_variable('tgain', tgain, 1.0D0, 500.0D0, &
                    'IFE target gain')
       case ('uccarb')
          call parse_real_variable('uccarb', uccarb, 10.0D0, 1.0D3, &
                    'Cost of carbon cloth ($/kg)')
       case ('ucconc')
          call parse_real_variable('ucconc', ucconc, 0.1D0, 1.0D3, &
                    'Cost of concrete ($/kg)')
       case ('ucflib')
          call parse_real_variable('ucflib', ucflib, 10.0D0, 1.0D3, &
                    'Cost of FLiBe ($/kg)')
       case ('uctarg')
          call parse_real_variable('uctarg', uctarg, 0.1D0, 1.0D3, &
                    'Cost per IFE target ($/target)')
       case ('v1dr')
          call parse_real_variable('v1dr', v1dr, 0.0D0, 10.0D0, &
                    'IFE void 1 radial thickness (m)')
       case ('v1dzl')
          call parse_real_variable('v1dzl', v1dzl, 0.0D0, 10.0D0, &
                    'IFE void 1 bottom part thickness (m)')
       case ('v1dzu')
          call parse_real_variable('v1dzu', v1dzu, 0.0D0, 10.0D0, &
                    'IFE void 1 top part thickness (m)')
       case ('v1matf')  !  N.B. actually a 2-D array
          call parse_real_array('v1matf', v1matf, isub1, 3*(maxmat+1), &
                    'IFE void 1 material fraction', icode)
       case ('v2dr')
          call parse_real_variable('v2dr', v2dr, 0.0D0, 10.0D0, &
                    'IFE void 2 radial thickness (m)')
       case ('v2dzl')
          call parse_real_variable('v2dzl', v2dzl, 0.0D0, 10.0D0, &
                    'IFE void 2 bottom part thickness (m)')
       case ('v2dzu')
          call parse_real_variable('v2dzu', v2dzu, 0.0D0, 10.0D0, &
                    'IFE void 2 top part thickness (m)')
       case ('v2matf')  !  N.B. actually a 2-D array
          call parse_real_array('v2matf', v2matf, isub1, 3*(maxmat+1), &
                    'IFE void 2 material fraction', icode)
       case ('v3dr')
          call parse_real_variable('v3dr', v3dr, 0.0D0, 50.0D0, &
                    'IFE void 3 radial thickness (m)')
       case ('v3dzl')
          call parse_real_variable('v3dzl', v3dzl, 0.0D0, 30.0D0, &
                    'IFE void 3 bottom part thickness (m)')
       case ('v3dzu')
          call parse_real_variable('v3dzu', v3dzu, 0.0D0, 30.0D0, &
                    'IFE void 3 top part thickness (m)')
       case ('v3matf')  !  N.B. actually a 2-D array
          call parse_real_array('v3matf', v3matf, isub1, 3*(maxmat+1), &
                    'IFE void 3 material fraction', icode)
     

       case default
          error_message = 'Unknown variable in input file: '//varnam(1:varlen)
          write(*,*) error_message
          write(*,*) 'Error occurred at this line in the IN.DAT file:', lineno
          write(*,*) line
          error = .True.

       end select variable

       !  Uncomment the following to abort the code if an obsolete variable name
       !  has been found in the input file

       if (obsolete_var) then
          error_message = 'Obsolete variable specified'
          write(*,*) error_message
          write(*,*) 'Error occurred at this line in the IN.DAT file: ', lineno
          write(*,*) line
          error = .True.
       end if

       !  If we have just read in an array, a different loop-back is needed

       if (icode == -1) goto 20

       cycle

    end do loop_over_lines

    if(neqns == 0) then
        ! The value of neqns has not been set in the input file.  Default = 0.
        neqns = no_constraints - nineqns
    else
        ! The value of neqns has been set in the input file.
        nineqns = no_constraints - neqns
    end if

    nvar = no_iteration

    if (error .eqv. .True.) stop

    ! MDK Try allocating here
    allocate(name_xc(nvar))

  end subroutine parse_input_file

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_real_variable(varnam,varval,vmin,vmax,description)

    !! Routine that obtains the value of a real variable from the input
    !! file and checks that it lies within the expected range
    !! author: P J Knight, CCFE, Culham Science Centre
    !! varnam : input string : name of the variable
    !! varval : input/output real : value of the variable
    !! vmin : input real : minimum allowed value for the variable
    !! vmax : input real : maximum allowed value for the variable
    !! description : input string : brief description of the variable
    !! This routine parses a line containing a 'name = value' pair
    !! for a real variable, extracting the value from the line
    !! and checking whether it lies between user-defined lower and
    !! upper limits.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: varnam, description
    real(dp), intent(inout) :: varval
    real(dp), intent(in) :: vmin, vmax

    !  Local variables

    real(dp) :: oldval

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME
    !  and stop if this is the case

    if (subscript_present) then
       write(*,*) 'Unexpected subscript found at line ', lineno
       write(*,*) 'Variable name and description:'
       write(*,*) varnam, ', ', description
          error = .True.
    end if

    !  Obtain the new value for the variable

    oldval = varval

    call get_value_real(varval,icode)

    if (icode /= 0) then
       write(*,*) 'Error whilst reading input file.  Variable name and description:'
       write(*,*) varnam, ', ', description
       write(*,*) 'Comments should be indicated by an asterisk'
       error = .True.
    end if

    !  Check variable lies within range

    call check_range_real(varnam,varval,vmin,vmax)

    if ((report_changes == 1).and.(varval /= oldval)) then
       write(outfile,*) trim(description),', ',trim(varnam),' = ',varval
    end if

  end subroutine parse_real_variable

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_int_variable(varnam,varval,vmin,vmax,description)

    !! Routine that obtains the value of an integer variable from the
    !! input file and checks that it lies within the expected range
    !! author: P J Knight, CCFE, Culham Science Centre
    !! varnam : input string : name of the variable
    !! varval : input/output integer : value of the variable
    !! vmin : input integer : minimum allowed value for the variable
    !! vmax : input integer : maximum allowed value for the variable
    !! description : input string : brief description of the variable
    !! This routine parses a line containing a 'name = value' pair
    !! for an integer variable, extracting the value from the line
    !! and checking whether it lies between user-defined lower and
    !! upper limits.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use constants, only: nout
    implicit none

    !  Arguments

    character(len=*), intent(in) :: varnam, description
    integer, intent(inout) :: varval
    integer, intent(in) :: vmin, vmax

    !  Local variables

    integer :: oldval

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME
    !  and stop if this is the case

    if (subscript_present) then
       write(*,*) 'Unexpected subscript found in IN.DAT at line number: ', lineno
       write(*,*) 'Name and description of variable: '
       write(*,*) varnam, description
       error = .True.
    end if

    !  Obtain the new value for the variable

    oldval = varval
    call get_value_int(varval,icode)
    if (icode /= 0) then
       write(*,*) 'Error found in input file, check line ',lineno
       write(*,*) 'Variable name, description:'
       write(*,*) varnam, ', ', description
          error = .True.
    end if

    !  Check variable lies within range

    call check_range_int(varnam,varval,vmin,vmax)

    if ((report_changes == 1).and.(varval /= oldval)) then
       write(outfile,*) trim(description),', ',trim(varnam),' = ',varval
    end if

  end subroutine parse_int_variable

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_string_variable(varnam,varval,description)

    !! Routine that obtains the value of a string variable from the
    !! input file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! varnam : input string : name of the variable
    !! varval : input/output string : value of the variable
    !! description : input string : brief description of the variable
    !! This routine parses a line containing a 'name = value' pair
    !! for a string variable, extracting the value from the line.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: varnam, description
    character(len=*), intent(inout) :: varval

    !  Local variables

    character(len=maxlen) :: oldval

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME
    !  and stop if this is the case

    if (subscript_present) then
       write(*,*) 'Unexpected subscript found in IN.DAT at line number: ', lineno
       write(*,*) 'Name and description of variable: '
       write(*,*) varnam, description
       error = .True.
       !stop
    end if

    !  Obtain the new value for the variable

    oldval = varval
    call get_substring(varval,icode)
    if (icode /= 0) then
       write(*,*) 'Error in IN.DAT found at line ',lineno
       write(*,*) 'Variable name, description:'
       write(*,*) varnam, ', ', description
       error = .True.
    end if

    if ((report_changes == 1).and.(trim(varval) /= trim(oldval))) then
       write(outfile,*) trim(description),', ',trim(varnam),' = ',varval
    end if

  end subroutine parse_string_variable

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_real_array(varnam,varval,isub1,n,description,icode)

    !! Routine that obtains the values of a real array from the input file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! varnam : input string : name of the variable
    !! varval(n) : input/output real array : value of the variable
    !! isub1 : input integer : array element pointer
    !! n : input integer : size of varval array
    !! icode : output integer : diagnostic flag
    !! description : input string : brief description of the variable
    !! This routine parses a line in one of the two following forms:
    !! <PRE>
    !! name = v1[, v2, ...]
    !! name(element) = v
    !! </PRE>
    !! to read in and extract one or more values for a real 1-D array.
    !! <P>N.B. No array bounds or value range checking is performed.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: varnam, description
    integer, intent(inout) :: isub1
    integer, intent(in) :: n
    integer, intent(out) :: icode
    real(dp), dimension(n), intent(inout) :: varval

    !  Local variables

    real(dp) :: oldval, val

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME

    if (subscript_present) then

       oldval = varval(isub1)
       call get_value_real(val,icode)

       if (icode /= 0) then
          write(*,*) 'Error in IN.DAT found at line ',lineno
          write(*,*) 'Variable name, description:'
          write(*,*) varnam, ', ', description
          error = .True.
       end if

       varval(isub1) = val
       if ((report_changes == 1).and.(varval(isub1) /= oldval)) then
          write(outfile,10) trim(description),', ', &
               trim(varnam),'(',isub1,') = ',varval(isub1)
       end if

    else

       isub1 = 1
       do
          call get_value_real(val,icode)
          !  icode == 1 denotes an error
          !  icode == -1 denotes end of line, so the next line needs to be read in
          !  (hence the 'goto 20' in the calling routine)
          if (icode /= 0) return

          oldval = varval(isub1)
          varval(isub1) = val
          if ((report_changes == 1).and.(varval(isub1) /= oldval)) then
             write(outfile,10) trim(description),', ', &
                  trim(varnam),'(',isub1,') = ',varval(isub1)
          end if
          isub1 = isub1 + 1
       end do
    end if

10  format(a,a,a,a1,i3,a,e14.6e2)

  end subroutine parse_real_array

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_int_array(varnam,varval,isub1,n,description,icode,startindex)

    !! Routine that obtains the values of an integer array
    !! from the input file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! varnam : input string : name of the variable
    !! varval(n) : input/output integer array : value of the variable
    !! isub1 : input integer : array element pointer
    !! n : input integer : size of varval array
    !! icode : output integer : diagnostic flag
    !! description : input string : brief description of the variable
    !! This routine parses a line in one of the two following forms:
    !! <PRE>
    !! name = v1[, v2, ...]
    !! name(element) = v
    !! </PRE>
    !! to read in and extract one or more values for an integer 1-D array.
    !! <P>N.B. No array bounds or value range checking is performed.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    character(len=*), intent(in) :: varnam, description
    integer, intent(inout) :: isub1
    integer, intent(in) :: n
    integer, intent(out) :: icode
    integer, dimension(n), intent(inout) :: varval
    integer, intent(in), optional :: startindex

    !  Local variables
    integer :: oldval, val
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME

    if (subscript_present) then

       oldval = varval(isub1)
       call get_value_int(val,icode)

       if (icode /= 0) then
          write(*,*) 'Error in IN.DAT found at line ',lineno
          write(*,*) 'Variable name, description:'
          write(*,*) varnam, ', ', description
          error = .True.
       end if

       varval(isub1) = val
       if ((report_changes == 1).and.(varval(isub1) /= oldval)) then
          write(outfile,10) trim(description),', ', &
               trim(varnam),'(',isub1,') = ',varval(isub1)
       end if

   else  ! subscript is not present

       isub1 = 1
       if(present(startindex))isub1 = startindex
       do
          call get_value_int(val,icode)

          !  icode == 1 denotes an error
          !  icode == -1 denotes end of line
          if (icode /= 0) then
              ! Make sure isub1 = the last array index
              isub1 = isub1 - 1
              return
          end if

          oldval = varval(isub1)
          varval(isub1) = val
          if ((report_changes == 1).and.(varval(isub1) /= oldval)) then
             write(outfile,10) trim(description),', ', &
                  trim(varnam),'(',isub1,') = ',varval(isub1)
          end if
          isub1 = isub1 + 1
       end do

    end if

10  format(a,a,a,a1,i3,a,i12)

  end subroutine parse_int_array

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine string_to_int(string,length,ivar,icode)

    !! Routine that converts the ASCII digits in a string to
    !! an integer
    !! author: P J Knight, CCFE, Culham Science Centre
    !! string : input string : contains digits of the number
    !! length : input integer : useful length of character string
    !! ivar : output integer : value stored in the string
    !! icode : output integer : diagnostic flag
    !! This routine converts the ASCII digits in string(1:length)
    !! to the integer ivar. It is equivalent to doing
    !! 'READ(STRING(1:LENGTH),I) IVAR' but this routine conforms
    !! to the ANSI standard.
    !! Each digit is parsed in turn, the current total is multiplied
    !! by ten and the new digit is added.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: string
    integer, intent(in) :: length
    integer, intent(out) :: ivar, icode

    !  Local variables

    character(len=maxlen) :: xstr
    integer :: iptr,izero,xlen
    logical :: negate

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ivar = 0
    icode = 0

    if (length <= 0) goto 1000

    negate = .false.
    izero = ichar('0')
    iptr = 1
    xstr = string(1:length)

    ! *** Ignore trailing spaces

    xlen = len_trim(xstr)
    if (xlen <= 0) goto 1000

    ! *** Ignore leading spaces

10  continue
    if (xstr(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr > xlen) goto 1000
       goto 10
    end if

    ! *** Check for leading + or -

    if (xstr(iptr:iptr) == '+') then
       iptr = iptr + 1
       if (iptr > xlen) goto 1000
    else if (xstr(iptr:iptr) == '-') then
       negate = .true.
       iptr = iptr + 1
       if (iptr > xlen) goto 1000
    else
       continue
    end if

    ! *** Ignore leading zeros

20  continue
    if (xstr(iptr:iptr) == '0') then
       iptr = iptr + 1
       if (iptr > xlen) goto 1000
       goto 20
    end if

    ! *** Check for number too large

    if ((xlen-iptr+1) > 10) then
       if (negate) then
          ivar = -1234567890
       else
          ivar = 1234567890
          write(*,*) '1 Problem with IN file, please check line'
          write(*,*) xstr
          error = .True.
       end if
       icode = 1
       goto 1000
    else if ((xlen-iptr+1) == 10) then
       if (xstr(iptr:xlen) > '2147483647') then
          if (negate) then
             ivar = -1234567890
          else
             ivar = 1234567890
          end if
          icode = 1
          goto 1000
       end if
    else
       continue
    end if

    ! *** Parse the digits

30  continue
    if ((xstr(iptr:iptr) >= '0').and.(xstr(iptr:iptr) <= '9')) then
       ivar = (ivar * 10) + (ichar(xstr(iptr:iptr))-izero)
       iptr = iptr + 1
       if (iptr <= xlen) goto 30

       ! *** This is the normal exit path...

       if (negate) ivar = -ivar

    else
       if(ivar /= 0) then
          write(*,*) 'Problem with IN file, please check line'
          write(*,*) xstr
          write(*,*) 'Comments should be indicated by an asterisk (*)'
          error = .True.
       end if
       icode = 1
    end if

1000 continue

  end subroutine string_to_int

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine string_to_real(string,length,rval,icode)

    !! Routine that converts the ASCII digits in a string to
    !! a real value
    !! author: P J Knight, CCFE, Culham Science Centre
    !! string : input string : contains digits of the number
    !! length : input integer : useful length of character string
    !! rvar : output real : value stored in the string
    !! icode : output integer : diagnostic flag
    !! This routine converts the ASCII digits in string(1:length)
    !! to the real variable rvar.
    !! The string is parsed one character at a time, from the left,
    !! handling the mantissa, and all other components of the real
    !! number separately, combining them at the end.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: string
    integer, intent(in) :: length
    real(dp), intent(out) :: rval
    integer, intent(out) :: icode

    !  Local variables

    real(dp) :: valbdp,valadp,xfact
    integer :: iptr,izero,iexpon
    logical :: negatm,negate

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    iptr = 1

    ! *** Ignore leading spaces

10  continue
    if (string(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= length) goto 10
    end if

    ! *** Initialise real value

    rval = 0.0D0

    ! *** ASCII '0'

    izero = ichar('0')

    ! *** If negative mantissa

    negatm = .false.

    ! *** If negative exponent

    negate = .false.

    ! *** Value before decimal point

    valbdp = 0.0D0

    ! *** Value after decimal point

    valadp = 0.0D0

    ! *** Exponent

    iexpon = 0

    ! *** First character can be +, -, ., or <digit>

    if (string(iptr:iptr) == '+') then
       iptr = iptr + 1
       if (iptr > length) goto 50
    else if (string(iptr:iptr) == '-') then
       iptr = iptr + 1
       if (iptr > length) goto 50
       negatm = .true.
    else
       continue
    end if

    ! *** Parse the mantissa - before the decimal point

    valbdp = 0.0D0
    xfact = 0.1D0
20  continue
    if ((string(iptr:iptr) >= '0').and.(string(iptr:iptr) <= '9')) then
       valbdp = (valbdp * 10.0D0) + dble(ichar(string(iptr:iptr))-izero)
       iptr = iptr + 1
       if (iptr > length) goto 50
       goto 20
    end if

    ! *** After the mantissa, we expect '.' or 'd' or 'e'

    if (string(iptr:iptr) == '.') then
       iptr = iptr + 1
       if (iptr > length) goto 50
    end if

    ! *** Parse the mantissa - after the decimal point

    valadp = 0.0D0
30  continue
    if ((string(iptr:iptr) >= '0').and.(string(iptr:iptr) <= '9')) then
       valadp = valadp + (dble(ichar(string(iptr:iptr))-izero)*xfact)
       xfact = xfact * 0.1D0
       iptr = iptr + 1
       if (iptr > length) goto 50
       goto 30
    end if

    ! *** Now we expect the exponent

    if ( (string(iptr:iptr) == 'D').or. &
         (string(iptr:iptr) == 'E').or. &
         (string(iptr:iptr) == 'd').or. &
         (string(iptr:iptr) == 'e')) then
       iptr = iptr + 1
       if (iptr > length) goto 50

       ! *** First character can be +, -, ., or <digit>

       if (string(iptr:iptr) == '+') then
          iptr = iptr + 1
          if (iptr > length) goto 50
       else if (string(iptr:iptr) == '-') then
          iptr = iptr + 1
          if (iptr > length) goto 50
          negate = .true.
       else
          continue
       end if

       ! *** Parse the exponent

40     continue
       if ((string(iptr:iptr) >= '0').and.(string(iptr:iptr) <= '9')) then
          iexpon = (iexpon * 10) + (ichar(string(iptr:iptr))-izero)
          iptr = iptr + 1
          if (iptr <= length) goto 40
       else
          goto 60
       end if
    else
       goto 60
    end if

50  continue

    ! *** Negative exponent?

    if (negate) iexpon = -iexpon

    ! *** Build the number at last

    if (iexpon == 0) then
       rval = (valbdp + valadp)
    else
       rval = (valbdp + valadp) * (10.0D0 ** iexpon)
    end if

    ! *** Negative mantissa?

    if (negatm) rval = -rval

    ! *** All OK

    icode = 0
    goto 1000

    ! *** Errors

60  continue

    write(*,*) 'Problem with IN file, please check line'
    write(*,*) string
    write(*,*) 'Comments should be indicated by an asterisk (*)'
    error = .True.

    icode = 1

1000 continue

  end subroutine string_to_real

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_value_int(ival,icode)

    !! Routine that extracts an integer value from a line of the
    !! input file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! ival   : output integer : extracted integer value
    !! icode  : output integer : diagnostic flag
    !! This routine extracts an integer value from the current line of
    !! the input file, i.e. the value of an integer variable as
    !! specified by the user.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: ival, icode

    !  Local variables

    character(len=maxlen) :: varval
    integer :: varlen
    integer :: foundComma, foundAst, foundPoint

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Ignore leading spaces

10  continue
    if (iptr <= linelen) then
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          goto 10
       end if
    end if

    if (iptr > linelen) goto 60

! 40     continue !KE I guess I can remove this too

    ! *** Put rest of line into varval (makes it easier to parse)

    varval = line(iptr:)

    ! *** Exclude any input after * or , - these denote an input comment

    varlen = len_trim(varval)
    foundComma = varlen
    foundAst = varlen
    foundPoint = 0

    if (index(varval,',') > 0) then
       foundComma = index(varval,',') - 1
    end if
    if (index(varval,'*') > 0) then
       foundAst = index(varval,'*') - 1
    end if
    varlen = min(varlen, foundComma, foundAst)

    if (varlen <= 0) varlen = index(varval,' ') - 1
    if (varlen <= 0) varlen = iptr

    varval = varval(:varlen)

    varlen = len_trim(varval)

    foundPoint = index(varval,'.') - 1
    if (foundPoint > 0) then
       varlen = foundPoint
       write(*,*) 'Integer value expected in following input line...'
       write(*,*) '   ',line(1:50),'...'
       error = .True.
    end if

    ! *** Update pointer

    iptr = iptr + varlen

    ! *** Ignore trailing spaces

50  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 50
    end if

    ! *** Ignore comma, if present

    if (iptr <= linelen) then
       if (line(iptr:iptr) == ',') iptr = iptr + 1
    end if

    ! *** Convert the ASCII text into an integer value

    call string_to_int(varval,varlen,ival,icode)

    goto 1000

60  continue
    icode = 1

1000 continue

  end subroutine get_value_int

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_value_real(rval,icode)

    !! Routine that extracts a real value from a line of the input file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! rval   : output real : extracted real value
    !! icode  : output integer : diagnostic flag
    !! This routine extracts a real value from the current line of
    !! the input file, i.e. the value of a real variable as specified
    !! by the user.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: icode
    real(dp), intent(out) :: rval

    !  Local variables

    character(len=maxlen) :: varval
    integer :: varlen
    integer :: foundComma, foundAst

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Ignore leading spaces

10  continue
    if (iptr <= linelen) then
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          goto 10
       end if
    end if
    if (iptr > linelen) goto 60

    ! *** Put rest of line into varval (makes it easier to parse)

    varval = line(iptr:)

    ! *** Exclude any input after * or , - these denote an input comment

    varlen = len_trim(varval)
    foundComma = varlen
    foundAst = varlen

    if (index(varval,',') > 0) then
       foundComma = index(varval,',') - 1
    end if
    if (index(varval,'*') > 0) then
       foundAst = index(varval,'*') - 1
    end if
    varlen = min(varlen, foundComma, foundAst)

    if (varlen <= 0) varlen = index(varval,' ') - 1
    if (varlen <= 0) varlen = iptr

    varval = varval(:varlen)

    varlen = len_trim(varval)

    ! *** Update pointer

    iptr = iptr + varlen

    ! *** Ignore trailing spaces

50  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 50
    end if

    ! *** Ignore comma, if present

    if (iptr <= linelen) then
       if (line(iptr:iptr) == ',') iptr = iptr + 1
    end if

    ! *** Convert the ASCII text into a real value

    call string_to_real(varval,varlen,rval,icode)

    goto 1000

60  continue
    icode = 1

1000 continue

  end subroutine get_value_real

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_substring(string,icode)

    !! Routine that extracts a substring from a line of the input file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! string : output string : extracted string
    !! icode  : output integer : diagnostic flag
    !! This routine extracts a string from the current line of
    !! the input file, i.e. the value of a string variable as specified
    !! by the user. Unlike routine
    !! <A HREF="get_substring_trim.html">get_substring_trim</A>,
    !! this routine does not truncate the string found at its first
    !! non-leading blank.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: icode
    character(len=*), intent(out) :: string

    !  Local variables

    character(len=maxlen) :: varval
    integer :: varlen

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Ignore leading spaces

10  continue
    if (iptr <= linelen) then
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          goto 10
       end if
    end if
    if (iptr > linelen) goto 60

    ! *** Put rest of line into varval (makes it easier to parse)

    varval = line(iptr:)
    varlen = len_trim(varval)

    if (varlen <= 0) varlen = iptr

    ! *** Update pointer

    iptr = iptr + varlen

    ! *** Ignore trailing spaces

50  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 50
    end if

    ! *** Ignore comma, if present

    if (iptr <= linelen) then
       if (line(iptr:iptr) == ',') iptr = iptr + 1
    end if

    ! *** Write the text into the variable

    string = varval

    goto 1000

60  continue
    icode = 1

1000 continue

  end subroutine get_substring

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_subscript(isub1,isub2,icode)

    !! Routine that extracts any subscripts present in a line of
    !! the input file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! isub1  : output integer : first subscript found
    !! isub2  : output integer : second subscript found
    !! icode  : output integer : diagnostic flag
    !! This routine extracts any subscripts from the current line of
    !! the input file, i.e. if any array elements are specified
    !! by the user. It looks at the next non-space character in the
    !! line, and if it is a left bracket, it assumes that at
    !! least one subscript is to follow and extracts it/them.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: isub1, isub2, icode

    !  Local variables

    integer :: izero
    logical :: negate

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Initial values

    isub1 = 0
    isub2 = 0

    ! *** First character should be '('

    if (line(iptr:iptr) /= '(') goto 70
    iptr = iptr + 1
    if (iptr > linelen) goto 80

    ! *** Parse the first subscript
    ! *** Ignore leading spaces

10  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr > linelen) goto 80
       goto 10
    end if

    izero = ichar('0')
    negate = .false.

    ! *** Extract and evaluate the first subscript
    ! *** Subscript may be prefaced by '+' or '-'

    if (line(iptr:iptr) == '+') then
       iptr = iptr + 1
       if (iptr > linelen) goto 80
    else if (line(iptr:iptr) == '-') then
       negate = .true.
       iptr = iptr + 1
       if (iptr > linelen) goto 80
    else
       continue
    end if

20  continue

    if ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) then
       isub1 = isub1 * 10 + ichar(line(iptr:iptr)) - izero
       iptr = iptr + 1
       if (iptr > linelen) goto 80
       goto 20
    end if
    if (negate) isub1 = -isub1

    ! *** Ignore trailing spaces of first subscript

30  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr > linelen) goto 70
       goto 30
    end if

    ! *** Is there a second subscript?

    if (line(iptr:iptr) == ',') then
       iptr = iptr + 1
       if (iptr > linelen) goto 80

       ! *** Ignore leading spaces of second subscript

40     continue
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          if (iptr > linelen) goto 80
          goto 40
       end if

       ! *** Extract and evaluate the second subscript

       negate = .false.

       ! *** Subscript may be prefaced by '+' or '-'

       if (line(iptr:iptr) == '+') then
          iptr = iptr + 1
          if (iptr > linelen) goto 80
       else if (line(iptr:iptr) == '-') then
          negate = .true.
          iptr = iptr + 1
          if (iptr > linelen) goto 80
       else
          continue
       end if
50     continue
       if ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) then
          isub2 = isub2 * 10 + ichar(line(iptr:iptr)) - izero
          iptr = iptr + 1
          if (iptr > linelen) goto 80
          goto 50
       end if

       ! *** Is it a negative subscript?

       if (negate) isub2 = -isub2

       ! *** Ignore trailing spaces of second subscript

60     continue
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          if (iptr <= linelen) goto 60
       end if

    end if

    ! *** Must end with ')'

    if (line(iptr:iptr) /= ')') goto 80
    iptr = iptr + 1

70  continue
    icode = 0
    goto 1000

80  continue
    icode = 1

1000 continue

  end subroutine get_subscript

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_variable_name(varnam,varlen,isub1,isub2)

    !! Routine that extracts a variable name from a line of
    !! the input file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! varnam : output string  : extracted variable name
    !! varlen : output integer : length of variable name
    !! isub1  : output integer : first subscript found
    !! isub2  : output integer : second subscript found
    !! This routine extracts a variable name from the current line of
    !! the input file. It also extracts any subscripts present.
    !! On exit, the counter <CODE>iptr</CODE> points to the first
    !! character of the value to be assigned to the variable.
    !! If the routine finds an error a value of 0 is returned in
    !! variable <CODE>varlen</CODE>.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: varlen, isub1, isub2
    character(len=*), intent(out) :: varnam

    !  Local variables

    character(len=maxlen) :: line1
    integer :: ifrom,ito,icode

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Store LINE in local variable

    line1 = line

    ! *** Convert string to lower case

    call lower_case(line)

    varlen = 0
    ifrom = iptr

    ! *** First character must be alphabetic

    if ((line(iptr:iptr) < 'a').or.(line(iptr:iptr) > 'z')) goto 1000
    iptr = iptr + 1
    if (iptr > linelen) goto 1000

    ! *** Now parse the rest of the letters (must be alphanumeric or _ )

10  continue
    if ( ((line(iptr:iptr) >= 'a').and.(line(iptr:iptr) <= 'z')).or. &
         ((line(iptr:iptr) == '_')).or. &
         ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) ) then
       iptr = iptr + 1
       if (iptr <= linelen) goto 10
    end if

    ! *** Extract variable name

    ito = iptr - 1
    varlen = ito - ifrom + 1
    if (varlen > 0) varnam = line(ifrom:ito)

    ! *** Ignore intervening spaces

20  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 20
    end if

    ! *** Now extract any subscript

    call get_subscript(isub1,isub2,icode)
    if (icode /= 0) then
       varlen = 0
       goto 1000
    end if

    ! *** Ignore intervening spaces

30  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 30
    end if

    ! *** We now expect '='

    if (line(iptr:iptr) == '=') then
       iptr = iptr + 1

       ! *** Restore original string's upper/lower case after '=' sign

       line(iptr:linelen) = line1(iptr:linelen)

    else
       varlen = 0
    end if

1000 continue

  end subroutine get_variable_name

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine check_range_int(cvar,varval,min_value,max_value)

    !! Routine that checks whether an integer variable lies within
    !! the desired range
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer  : Fortran output unit identifier
    !! cvar    : input string   : name of variable
    !! varval  : input integer  : value of variable
    !! min_value : input integer : minimum allowed value of variable
    !! max_value : input integer : maximum allowed value of variable
    !! This routine checks whether an integer variable lies within
    !! the range predetermined by the user, and reports an error
    !! and stops if it doesn't.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: cvar
    integer, intent(in) :: varval,min_value,max_value

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (min_value > max_value) then
       write(outfile,*) 'Illegal relative values of min_value and max_value'
       write(outfile,*) 'for variable ',cvar

       write(*,*) 'Illegal relative values of min_value and max_value'
       write(*,*) 'for variable ',cvar
          error = .True.
    end if

    if ((varval < min_value).or.(varval > max_value)) then
       write(outfile,*) cvar,' lies outside its allowed range :'
       write(outfile,*) 'Minimum value = ',min_value
       write(outfile,*) 'Maximum value = ',max_value
       write(outfile,*) ' Actual value = ',varval

       write(*,*) cvar,' lies outside its allowed range :'
       write(*,*) 'Minimum value = ',min_value
       write(*,*) 'Maximum value = ',max_value
       write(*,*) ' Actual value = ',varval
          error = .True.
    end if

  end subroutine check_range_int

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine check_range_real(cvar,varval,min_value,max_value)

    !! Routine that checks whether a real variable lies within
    !! the desired range
    !! author: P J Knight, CCFE, Culham Science Centre
    !! cvar    : input string   : name of variable
    !! varval  : input real     : value of variable
    !! min_value : input real   : minimum allowed value of variable
    !! max_value : input real   : maximum allowed value of variable
    !! This routine checks whether a real variable lies within
    !! the range predetermined by the user, and reports an error
    !! and stops if it doesn't.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: cvar
    real(dp), intent(in) :: varval,min_value,max_value

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (min_value > max_value) then
       write(outfile,*) 'Illegal relative values of min_value and max_value'
       write(outfile,*) 'for variable ',cvar

       write(*,*) 'Illegal relative values of min_value and max_value'
       write(*,*) 'for variable ',cvar
          error = .True.
    end if

    if ((varval < min_value).or.(varval > max_value)) then
       write(outfile,*) cvar,' lies outside its allowed range :'
       write(outfile,*) 'Minimum value = ',min_value
       write(outfile,*) 'Maximum value = ',max_value
       write(outfile,*) ' Actual value = ',varval

       write(*,*) cvar,' lies outside its allowed range :'
       write(*,*) 'Minimum value = ',min_value
       write(*,*) 'Maximum value = ',max_value
       write(*,*) ' Actual value = ',varval
          error = .True.
    end if

  end subroutine check_range_real


  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine lower_case(string,start,finish)

   !! Routine that converts a (sub-)string to lowercase
   !! author: P J Knight, CCFE, Culham Science Centre
   !! string : input string   : character string of interest
   !! start  : optional input integer  : starting character for conversion
   !! finish : optional input integer  : final character for conversion
   !! This routine converts the specified section of a string
   !! to lowercase. By default, the whole string will be converted.
   !! None
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

   !  Arguments

   character(len=*), intent(inout) :: string
   integer, optional, intent(in) :: start,finish

   !  Local variables

   character(len=1) :: letter
   character(len=27) :: lowtab = 'abcdefghijklmnopqrstuvwxyz_'
   integer :: loop, i

   integer :: first, last

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   if (present(start)) then
      first = start
   else
      first = 1
   end if

   if (present(finish)) then
      last = finish
   else
      last = len(string)
   end if

   if (first <= last) then
      do loop = first,last
         letter = string(loop:loop)
         i = index('ABCDEFGHIJKLMNOPQRSTUVWXYZ_',letter)
         if (i > 0) string(loop:loop) = lowtab(i:i)
      end do
   end if

 end subroutine lower_case

end module process_input

#ifdef unit_test
program test
 use process_input
 implicit none

 open(unit=1,file='IN.DAT',status='old')
 call parse_input_file(1,6,1)
 close(unit=1)
end program test
#endif
