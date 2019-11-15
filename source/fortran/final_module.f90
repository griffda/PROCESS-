module final_module

contains


! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine final(ifail)

  !+ad_name  final
  !+ad_summ  Routine to print out the final point in the scan
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail   : input integer : error flag
  !+ad_desc  This routine prints out the final point in the scan.
  !+ad_prob  None
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  oheadr
  !+ad_call  output
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  23/01/13 PJK Changed format for single iteration outputs
  !+ad_hist  10/09/14 PJK Removed output lines if a given solver is unused
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output
  use numerics
  use output_module!, only:output
  use constraints
  implicit none

  !  Arguments
  integer, intent(in) :: ifail
  integer :: inn
  real(kind(1.0D0)), dimension(ipeqns) :: con1, con2, err
  character(len=1), dimension(ipeqns) :: sym
  character(len=10), dimension(ipeqns) :: lab

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (ifail == 1) then
     call oheadr(nout,'Final Feasible Point')
  else
     call oheadr(nout,'Final UNFEASIBLE Point')
  end if

  ! check if no optimisation will be done, if so 
  ! compute the OP variables now
  if (ioptimz == -2) then
    call loadxc
    call caller(xcm,nvar)
    !call funfom(objfun)

    call oheadr(nout,'Numerics')
    call ocmmnt(nout,'PROCESS has performed a run witout optimisation.')
    call oblnkl(nout)

    ! Print the residuals of the constraint equations
    call constraint_eqns(neqns+nineqns,con1,-1,con2,err,sym,lab)
    write(nout,120)
    120 format(t48,'physical',t73,'constraint',t100,'normalised')
    write(nout,130)
    130 format(t47,'constraint',t74,'residue',t101,'residue')
    call oblnkl(nout)
    do inn = 1,neqns
        write(nout,140) inn,lablcc(icc(inn)),sym(inn),con2(inn), &
            lab(inn),err(inn),lab(inn),con1(inn)
       call ovarre(mfile,lablcc(icc(inn))//' normalised residue', &
           '(normres'//int_to_string3(inn)//')',con1(inn))
    end do

    140 format(t2,i4,t8,a33,t46,a1,t47,1pe12.4,t60,a10,t71,1pe12.4,t84,a10,t98,1pe12.4)

    if (nineqns > 0) then
        call osubhd(nout, &
                    'The following inequality constraint residues should be greater than or approximately equal to zero :')

        do inn = neqns+1,neqns+nineqns
            write(nout,140) inn,lablcc(icc(inn)),sym(inn),con2(inn), &
                    lab(inn), err(inn), lab(inn)
            call ovarre(mfile,lablcc(icc(inn)),'(constr'//int_to_string3(inn)//')',rcm(inn))
        end do
    end if
  end if

  call output(nout)

  if (nfev1 == 0) then  !  no HYBRD call
     !if (nviter == 1) then
     !    write(iotty,10) nviter,ncalls
     !else
    !    write(iotty,20) nviter,ncalls
     !end if
  else if (nviter == 0) then  !  no VMCON call
     if (nfev1 == 1) then
        write(iotty,30) nfev1,ncalls
     else
        write(iotty,40) nfev1,ncalls
     end if
  else if (nfev1 == 1) then ! (unlikely that nviter is also 1...)
     write(iotty,50) nfev1,nviter,ncalls
  else if (nviter == 1) then ! (unlikely that nfev1 is also 1...)
     write(iotty,60) nfev1,nviter,ncalls
  else
     write(iotty,70) nfev1,nviter,ncalls
  end if

30 format( &
       t2,'The HYBRD point required ',i5,' iteration',/, &
       t2,'There were ',i6,' function calls')
40 format( &
       t2,'The HYBRD point required ',i5,' iterations',/, &
       t2,'There were ',i6,' function calls')
50 format( &
       t2,'The HYBRD point required ',i5,' iteration',/, &
       t2,'The optimisation required ',i5,' iterations',/, &
       t2,'There were ',i6,' function calls')
60 format( &
       t2,'The HYBRD point required ',i5,' iterations',/, &
       t2,'The optimisation required ',i5,' iteration',/, &
       t2,'There were ',i6,' function calls')
70 format( &
       t2,'The HYBRD point required ',i5,' iterations',/, &
       t2,'The optimisation required ',i5,' iterations',/, &
       t2,'There were ',i6,' function calls')

end subroutine final


end module final_module
