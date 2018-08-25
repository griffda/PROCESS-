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
  implicit none

  !  Arguments
  integer, intent(in) :: ifail

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (ifail == 1) then
     call oheadr(nout,'Final Feasible Point')
  else
     call oheadr(nout,'Final UNFEASIBLE Point')
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
