! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine initial

    !+ad_name  initial
    !+ad_summ  Routine to initialise
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_prob  None
    !+ad_call  process_output
    !+ad_call  stellarator_module
    !+ad_call  stellarator_variables
    !+ad_call  devtyp
    !+ad_call  stinit
    !+ad_hist  08/10/96 PJK Initial upgraded version
    !+ad_hist  20/01/97 PJK Added htpmw
    !+ad_hist  22/01/97 PJK Subsumed heattr.h, heatrinp.h and pfelect.h into
    !+ad_hisc               htpwr.h
    !+ad_hist  05/02/97 PJK Added Martensitic steel fractions fms...
    !+ad_hist  13/02/97 PJK Changed initial value of fwlife to zero (no
    !+ad_hisc               longer an input parameter, but calculated)
    !+ad_hist  21/03/97 PJK Added new iteration variables 81--86
    !+ad_hist  18/11/97 PJK Added ITER-97 scaling laws, IVMS,DRTOP,DZTOP
    !+ad_hist  01/04/98 PJK Added ITER-96P scaling law, IGNITE, DNLA
    !+ad_hist  24/04/98 PJK Added IMPC,IMPFE,IMPO,FKBLKT
    !+ad_hist  17/07/98 PJK Added PTHRMW
    !+ad_hist  08/10/98 PJK Added new ITER H98 scaling laws
    !+ad_hist  19/01/99 PJK Added IGEOM, POWERHT
    !+ad_hist  19/05/99 PJK Added new availability model variables
    !+ad_hist  06/07/99 PJK Added BCRITSC, JCRITSC, TCRITSC
    !+ad_hist  25/04/02 PJK Added ZEFFDIV
    !+ad_hist  15/06/04 PJK Added IPRIMHTP
    !+ad_hist  22/05/06 PJK Added IFALPHAP
    !+ad_hist  22/05/07 PJK Added hydrogen plant variables
    !+ad_hist  21/03/11 PJK Changed default value of FEFFCD to 1.0
    !+ad_hist  26/07/11 PJK Added JCRIT_MODEL
    !+ad_hist  09/11/11 PJK Removed ICULCR
    !+ad_hist  19/09/12 PJK Initial F90 version
    !+ad_hist  05/11/12 PJK Removed call to ifeini
    !+ad_hist  05/11/12 PJK Removed pulsed reactor variables
    !+ad_hist  05/03/15 JM  Changed blanket fraction check to new models
    !+ad_hist  26/08/15 MDK Added check that constraint 63 is not used with wrong vacuum model
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use process_output
    use stellarator_module
    use stellarator_variables

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  See which type of device is being modelled

    call devtyp

    !  Initialise stellarator parameters if necessary

    if (istell == 1) call stinit

end subroutine initial

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine devtyp

    !+ad_name  devtyp
    !+ad_summ  Routine to determine which type of device is to be modelled
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine uses the contents of an input file,
    !+ad_desc  <CODE>device.dat</CODE>, to determine which type of device
    !+ad_desc  is to be modelled. If the file is not present in the current
    !+ad_desc  directory, a standard tokamak model is assumed.
    !+ad_prob  None

    !+ad_call  stellarator_variables
    !+ad_hist  27/02/96 PJK Initial version
    !+ad_hist  08/10/96 PJK Fixed error: (istell.gt.2) should be (idev.gt.2)
    !+ad_hist  14/03/97 PJK idev=3 ==> inertial fusion power plant
    !+ad_hist  19/09/12 PJK Initial F90 version
    !+ad_hist  04/11/16 MK Added check for content of device file
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    use stellarator_variables

    implicit none

    !  Local variables

    integer :: idev
    integer :: iost
    logical :: iexist
    character(len = 20) :: devFile
    character(len = 5) :: line
    line = ' '
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    devFile = 'device.dat'
    istell = 0
    idev   = 0      ! Default value MK

    !  Read a second input file. If the file does not exist or
    !  blank, then the standard tokamak option is assumed.

    inquire(file = devFile, exist = iexist)

    if (iexist) then
        open(unit = 101, file = 'device.dat', status = 'old')
        DO
            read(101,'(A)', IOSTAT = iost) line
            read(line, '(I2)') idev
            if(iost < 0 .or. idev > 0) exit
        END DO
        close(unit = 101)

        !  Set relevant switch

        select case (idev)

        case (1)  !  Stellarator model
            istell = 1

        case (2)  !  ! ISSUE #508 Remove RFP option
        case (3)  !  ISSUE #508 Remove Inertial Fusion Energy model

        case default  !  Tokamak model
            continue

        end select
    end if

end subroutine devtyp

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine check

    !+ad_name  check
    !+ad_summ  Routine to reset specific variables if certain options are
    !+ad_summ  being used
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine performs a sanity check of the input variables
    !+ad_desc  and ensures other dependent variables are given suitable values.
    !+ad_prob  None
    !+ad_call  build_variables
    !+ad_call  buildings_variables
    !+ad_call  current_drive_variables
    !+ad_call  error_handling
    !+ad_call  fwbs_variables
    !+ad_call  global_variables
    !+ad_call  heat_transport_variables

    !+ad_call  numerics
    !+ad_call  pfcoil_variables
    !+ad_call  physics_variables
    !+ad_call  process_output
    !+ad_call  pulse_variables
    !+ad_call  tfcoil_variables
    !+ad_call  report_error
    !+ad_hist  08/10/96 PJK Initial upgraded version
    !+ad_hist  23/01/97 PJK Moved resetting of trithtmw from POWER
    !+ad_hist  01/04/98 PJK Added rnbeam reset for no NBI
    !+ad_hist  19/01/99 PJK Added warning about iiter flag with non-ITER profiles
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  18/12/12 PJK Added snull and other PF coil location checks
    !+ad_hist  11/04/13 PJK Energy storage building volume set to zero if lpulse=0
    !+ad_hist  23/05/13 PJK Coolant type set to water if blktmodel>0
    !+ad_hist  10/06/13 PJK Removed enforcement of ishape=0 for non-TART tokamaks
    !+ad_hist  11/09/13 PJK Added check for fuel ion fractions; removed idhe3 setting;
    !+ad_hisc               removed iiter usage
    !+ad_hist  08/05/14 PJK Replaced itfmod with tfc_model
    !+ad_hist  13/05/14 PJK Added impurity fraction initialisations
    !+ad_hist  02/06/14 PJK Added fimpvar usage
    !+ad_hist  24/06/14 PJK Removed refs to bcylth
    !+ad_hist  26/06/14 PJK Added error_handling
    !+ad_hist  23/07/14 PJK Modified icase descriptions
    !+ad_hist  19/08/14 PJK Added trap for nvar < neqns
    !+ad_hist  01/09/14 PJK Added trap for insufficient specification of ixc, icc
    !+ad_hist  08/09/14 PJK Changed costr to coolwh
    !+ad_hist  15/09/14 PJK Added plasma pedestal consistency checks
    !+ad_hist  23/10/14 PJK ipowerflow=0 and blkttype=3 for KIT blanket model
    !+ad_hist  29/10/14 PJK Ensured constraint 42 no longer used
    !+ad_hist  17/11/14 PJK Added trap for deprecated constraints 3,4
    !+ad_hist  24/11/14 PJK Added trap if blanket material fractions do not sum to 1.0
    !+ad_hist  24/11/14 PJK Set coolwh via blkttype
    !+ad_hist  25/02/15 JM  Changed blanket composition check to use new blanket model layout
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables
    use buildings_variables
    use current_drive_variables
    use error_handling
    use fwbs_variables
    use global_variables
    use heat_transport_variables
    use impurity_radiation_module

    use numerics
    use pfcoil_variables
    use physics_variables
    use plasmod_variables
    use process_output
    use pulse_variables
    use tfcoil_variables
    use stellarator_variables
    use sctfcoil_module
    use vacuum_variables

    implicit none

    !  Local variables

    integer :: i,j,k,imp
    real(kind(1.0D0)) :: fsum


    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    errors_on = .true.

    !  Check that there are sufficient iteration variables

    if (nvar < neqns) then
        idiags(1) = nvar ; idiags(2) = neqns
        call report_error(137)
    end if

    !  Check that sufficient elements of ixc and icc have been specified

    if ( any(ixc(1:nvar) == 0) ) then
        idiags(1) = nvar
        call report_error(139)
    end if


    if ( any(icc(1:neqns+nineqns) == 0) ) then
        idiags(1) = neqns ; idiags(2) = nineqns
        call report_error(140)
    end if

    !  Deprecate constraints 3 and 4

    if ( any(icc(1:neqns+nineqns) == 3) ) then
        call report_error(162)
    end if

    if ( any(icc(1:neqns+nineqns) == 4) ) then
        call report_error(163)
    end if

    ! MDK Report error is constraint 63 is used with old vacuum model
    if (any(icc(1:neqns+nineqns) == 63).and.(vacuum_model.ne.'simple') ) then
        write(*,*) 'Constraint 63 is requested without the correct vacuum model ("simple").'
        write(*,*) 'vacuum_model = ', vacuum_model
        write(*,*) 'PROCESS stopping'
        stop
    end if

    !  Fuel ion fractions must add up to 1.0

    if (abs(1.0D0 - fdeut - ftrit - fhe3) > 1.0D-6) then
        fdiags(1) = fdeut; fdiags(2) = ftrit ; fdiags(3) = fhe3
        call report_error(36)
    end if

    if (ftrit < 1.0D-3) then  !  tritium fraction is negligible
        triv = 0.0D0
        ifispact = 0
        trithtmw = 0.0D0
    end if

    !  Impurity fractions

    if (imprad_model == 1) then
        do imp = 1,nimp
            impurity_arr(imp)%frac = fimp(imp)
        end do
    end if


    !  Warn if ion power balance equation is being used with the new radiation model

    if ((imprad_model == 1).and.(any(icc == 3))) then
        call report_error(138)
    end if

    !  Plasma profile consistency checks

    if (ipedestal == 1 .or. ipedestal == 2) then

        !  Temperature checks

        if (teped < tesep) then
            fdiags(1) = teped ; fdiags(2) = tesep
            call report_error(146)
        end if

        if ((abs(rhopedt-1.0D0) <= 1.0D-7).and.((teped-tesep) >= 1.0D-7)) then
            fdiags(1) = rhopedt ; fdiags(2) = teped ; fdiags(3) = tesep
            call report_error(147)
        end if

        !  Core temperature should always be calculated (later) as being
        !  higher than the pedestal temperature, if and only if the
        !  volume-averaged temperature never drops below the pedestal
        !  temperature. Prevent this by adjusting te, and its lower bound
        !  (which will only have an effect if this is an optimisation run)

        if (te <= teped) then
            fdiags(1) = te ; fdiags(2) = teped
            te = teped*1.001D0
            call report_error(149)
        end if

        if ((ioptimz >= 0).and.(any(ixc == 4)).and.(boundl(4) < teped*1.001D0)) then
            call report_error(150)
            boundl(4) = teped*1.001D0
            boundu(4) = max(boundu(4), boundl(4))
        end if

        !  Density checks:
        !  not required if pedestal is set using Greenwald density (Issue #292)

        if ((fgwped < 0) .and. (neped < nesep)) then
            ! Issue #589 Pedestal density is set manually using neped but it is less than nesep.
            fdiags(1) = neped ; fdiags(2) = nesep
            call report_error(151)
        end if

        if ((fgwped < 0) .and. (abs(rhopedn-1.0D0) <= 1.0D-7).and.((neped-nesep) >= 1.0D-7)) then
            ! Issue #589 Pedestal density is set manually using neped,
            ! but pedestal width = 0.
            fdiags(1) = rhopedn ; fdiags(2) = neped ; fdiags(3) = nesep
            call report_error(152)
        end if

        !  Core density should always be calculated (later) as being
        !  higher than the pedestal density, if and only if the
        !  volume-averaged density never drops below the pedestal
        !  density. Prevent this by adjusting dene, and its lower bound
        !  (which will only have an effect if this is an optimisation run)
        !  Not required if pedestal is set using Greenwald density (Issue #292)

        if ((fgwped < 0) .and. (dene <= neped)) then
            fdiags(1) = dene ; fdiags(2) = neped
            write(*,*)'dene = ', dene, 'boundl(6) = ', boundl(6), '  neped = ', neped
            write(*,*)'Set dene = neped*1.001D0 '
            dene = neped*1.001D0
            call report_error(154)
         end if
         
        if ((ioptimz >= 0).and.(any(ixc == 6)).and.(boundl(6) < neped*1.001D0)) then
            call report_error(155)
            write(*,*)'dene = ', dene, 'boundl(6) = ', boundl(6), '  neped = ', neped
            write(*,*)'Set boundl(6) = neped*1.001D0'
            boundl(6) = neped*1.001D0
            boundu(6) = max(boundu(6), boundl(6))
        end if

     end if
     
     
     ! Cannot use Psep/R and PsepB/qAR limits at the same time
     if(any(icc == 68) .and. any(icc == 56)) then
        call report_error(178)
     endif
     

     if(ipedestal == 2 .or. ipedestal == 3) then

        !PLASMOD automatically takes both pseprmax and psepbqarmax as input
        !It uses which ever one leads to the lower Psep value. Not to use
        !one of them set them to a very large number
        call report_error(199)
        
        if (fgwped .le. 0 )then
           call report_error(176)
        endif
        if (fgwsep .le. 0 ) then
           call report_error(177)
        endif


        !need to enforce H-mode using Martin scaling, if using PLASMOD
        !HL Todo: The L-H threshold is checked inside PLASMOD do we need to duplicate in PROCESS??
        !if(.not. any(icc == 15)) then
     !      call report_error(179)
        !endif

        if (boundl(103) < 1.) then
           call report_error(181)
        endif
        
        ! Initialise value for gamcd for use in PLASMOD first iteration
        gamcd = 0.3


        ! PLASMOD only uses the core radiation for the H-factor correction.
        ! It calculates the power balance using the total radiation.
        if(iradloss .ne. 0) then
           call report_error(184)
        endif

        ! Mutually exclusive variables - issue #632
        if ((plasmod_contrpovs.ne.0).and.(plasmod_contrpovr.ne.0))then
           call report_error(187)
        endif

        !kappa95 and triang95 are input to PLASMOD
        !kappa and triang are calculated
        if (ishape .ne. 4) then
           call report_error(196)
        endif

        !PLASMOD uses its own NBI current drive routine
        if (.not.((iefrf == 5) .or. (iefrf == 8)) )then
           call report_error(197)
        endif


        !PLASMOD always uses current drive
        if(irfcd == 0) then
           call report_error(198)
        endif

       
     endif

     if ((any(ixc==145)) .and. (boundl(145) < fgwsep)) then  !if lower bound of fgwped < fgwsep
        fdiags(1) = boundl(145); fdiags(2) = fgwsep
        call report_error(186)
     end if

     if(ipedestal==3)then
        ! Stop PROCESS if certain iteration variables have been requested while using PLASMOD
        ! These are outputs of PLASMOD
        ! ixc(4) = te, ixc(5) = beta, ixc(6) = dene, ixc(9) = fdene,
        ! ixc(102) = fimpvar, ixc(103) = flhthresh, ixc(109) = ralpne,
        ! ixc(110) = ftaulimit, ixc(44) = fvsbrnni
        if(any((ixc==4).or.(ixc==5).or.(ixc==6).or.(ixc==9).or. &
             (ixc==102).or.(ixc==103).or.(ixc==109).or.(ixc==110)&
             .or.(ixc==44)))then
           call report_error(182)
        endif

        !as beta is an output of PLASMOD, its consistency does not need to be enforced
        if(any(icc == 1)) then
           call report_error(188)
        endif
           
        ! density limit cannot be used with PLASMOD use fgwsep and/or fgwped instead.
        if (any(icc==9)) then
           call report_error(183)
        endif

        ! The global power balance cannot be enforced when running PLASMOD
        ! PROCESS cannot vary any of the relevant inputs as these are all
        ! outputs of PLASMOD issue #631
        if (any(icc == 2)) then
           call report_error(185)
        endif



        if (plasmod_i_equiltype == 2) then

           !Warning, this is a not recommended option
           !operation is inconsistent with PROCESS.
           call report_error(189)

           !cannot use q as iteration variable, if plasma current is input for EMEQ
           if (any(ixc == 18)) then
              call report_error(190)
           endif           
        endif

        !plasmod_i_modeltype = 1 enforces a given H-factor
        !plasmod_i_modeltype > 1 calculates transport from the transport models
        if (plasmod_i_modeltype > 1) then

           !beta limit is enforced inside PLASMOD
           !icc(6)  eps * betap upper limit
           !ixc(8)  fbeta
           !icc(24) beta upper limit
           !ixc(36) fbetatry
           !icc(48) poloidal beta upper limit
           !ixc(79) fbetap
           if (any((icc == 6) .or. (icc == 24) .or. (icc==48) ) .or. any((ixc == 8) .or.(ixc==36) .or. (ixc==79))) then
              call report_error(191)
           endif
           
        endif
        
     endif
     

    !  Tight aspect ratio options
    ! ---------------------------

    if (itart == 1) then

        icase  = 'Tight aspect ratio tokamak model'
        bore   = 0.0D0
        gapoh  = 0.0D0
        ohcth  = 0.0D0
        ddwi   = 0.0D0

        if (icurr /= 2) then
            idiags(1) = icurr ; call report_error(37)
        end if
        iohcl  = 0
        ipfloc(1) = 2
        ipfloc(2) = 3
        ipfloc(3) = 3
        itfsup = 0

        if (ibss == 1) call report_error(38)
        if (snull == 1) call report_error(39)

    else

        if (icurr == 2) call report_error(40)

        if (snull == 0) then
            idivrt = 2
        else  !  snull == 1
            idivrt = 1
        end if

        !  Check PF coil configurations

        j = 0 ; k = 0
        do i = 1, ngrp
            if ((ipfloc(i) /= 2).and.(ncls(i) /= 2)) then
                idiags(1) = i ; idiags(2) = ncls(i)
                call report_error(41)
            end if

            if (ipfloc(i) == 2) then
                j = j + 1
                k = k + ncls(i)
            end if
        end do

        if (k == 1) call report_error(42)
        if (k > 2) call report_error(43)
        if ((snull == 1).and.(j < 2)) call report_error(44)

    end if

    !  Pulsed power plant model

    if (lpulse == 1) then
        icase = 'Pulsed tokamak model'
    else
        esbldgm3 = 0.0D0
    end if

    !  Ensure minimum cycle time constraint is turned off
    !  (not currently available, as routine thrmal has been commented out)

    if ( any(icc == 42) ) then
        call report_error(164)
    end if

    !  Ensure that if TF coils are non-superconducting,
    !  only simple stress calculations are performed
    if (itfsup == 0) tfc_model = 0

    ! TF coil
    ! -------

    ! If TFC sidewall has not been set by user
    if(casths<0.1d-10) tfc_sidewall_is_fraction = .true.
    ! If inboard TF coil case plasma side thickness has not been set by user
    if(casthi<0.1d-10) casthi_is_fraction = .true.

    ! Issue #514 Radial dimensions of inboard leg
    ! Ensure that tfcth is defined if thkwp is an iteration variable (140)
    ! if (any(ixc(1:nvar) == 140) ) then
    !     tfcth = thkwp + casthi + thkcas + 2.0D0*tinstf + 2.0d0*tfinsgap
    ! endif

    !  PF coil resistivity is zero if superconducting
    if (ipfres == 0) pfclres = 0.0D0

    !  If there is no NBI, then hot beam density should be zero
    if (irfcd == 1) then
        if ((iefrf /= 5).and.(iefrf /= 8)) rnbeam = 0.0D0
    else
        rnbeam = 0.0D0
    end if

    !  Solid breeder assumed if ipowerflow=0

    !if (ipowerflow == 0) blkttype = 3

    !  Set coolant fluid type

    !if ((blkttype == 1).or.(blkttype == 2)) then
    !   coolwh = 2  !  water
    !else
    !   coolwh = 1  !  helium
    !end if

    !  But... set coolant to water if blktmodel > 0
    !  Although the *blanket* is by definition helium-cooled in this case,
    !  the shield etc. are assumed to be water-cooled, and since water is
    !  heavier (and the unit cost of pumping it is higher), the calculation
    !  for coolmass is better done with coolwh=2 if blktmodel > 0 to give
    !  slightly pessimistic results.

    !if (blktmodel > 0) then
    !   secondary_cycle = 0
    !   blkttype = 3  !  HCPB
    !   coolwh = 2
    !end if

    !  Ensure that blanket material fractions allow non-zero space for steel
    !  CCFE HCPB Model

    if (istell == 0) then
        if ((iblanket == 1).or.(iblanket == 3)) then
            fsum = breeder_multiplier + vfcblkt + vfpblkt
            if (fsum >= 1.0D0) then
                idiags(1) = iblanket
                fdiags(2) = breeder_multiplier
                fdiags(3) = vfcblkt
                fdiags(4) = vfpblkt
                fdiags(5) = fsum
                call report_error(165)
            end if
        end if
    end if

    ! Initialise superconductor cable parameters
    if(itfsup==1)then
        call initialise_cables()
    end if

    ! Check that the temperature margins are not overdetermined
    if(tmargmin>0.0001d0)then
        ! This limit has been input and will be applied to both TFC and CS
        if(tmargmin_tf>0.0001d0)then
            write(*,*)'tmargmin_tf and tmargmin should not both be specified in IN.DAT.'
            write(*,*)'tmargmin_tf has been ignored.'
        end if
        if(tmargmin_cs>0.0001d0)then
            write(*,*)'tmargmin_cs and tmargmin should not both be specified in IN.DAT.'
            write(*,*)'tmargmin_cs has been ignored.'
        end if
        tmargmin_tf = tmargmin
        tmargmin_cs = tmargmin
     end if


     
    errors_on = .false.

end subroutine check
