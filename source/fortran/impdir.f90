module impdir
! temporary module to allow input.f90 and impurity_radiation.f90 to share this function
! without cyclic dependencies during the conversion of impurity_radiation.f90.
contains
    character(len=300) function getimpdir()
        implicit none
        character(len=200) :: process_dir
        CALL get_environment_variable("PYTHON_PROCESS_ROOT", process_dir)
        if (process_dir == "") then
            getimpdir = INSTALLDIR//'/process/data/impuritydata/'
        else
            getimpdir = trim(process_dir)//'/data/impuritydata/'
        end if
    end function getimpdir

end module impdir