#   GFortranLibs Finder
#   Author  :   K. Zarebski
#   Date    :   last modified 2020-11-06
#
#   Get the location of the GFortran Libraries so they can
#   be included with the Python module
#   TODO: Ideally we do not want to do this (likely be removed
#   when f90wrap is dropped)


MACRO(GET_GFORTRANLIBS)
    SET(GFORTLIB_NAME "libgfortran")
    EXECUTE_PROCESS(
        COMMAND bash -c "$(which gcc) -lgfortran -c -v 2>&1 >/dev/null | grep COMPILER_PATH | cut -d '=' -f 2 "
        OUTPUT_VARIABLE GFORTRAN_LIBRARY_DIRS
    )
    STRING(REGEX MATCHALL "([^\:])+\/lib\/" LIB_LOCS "${GFORTRAN_LIBRARY_DIRS}")
    LIST(REMOVE_DUPLICATES LIB_LOCS)
    FOREACH(LOC ${LIB_LOCS})
        FILE(GLOB_RECURSE GFORTSEARCH ${LOC}libgfortran*${LIBRARY_OUTPUT_SUFFIX})
        LIST(LENGTH GFORTSEARCH NRES)
        IF(GFORTSEARCH)
            IF(NRES GREATER 1)
                LIST(GET GFORTSEARCH 1 GFORTRAN_LIBRARY)
            ELSE()
                SET(GFORTRAN_LIBRARY ${GFORTSEARCH})
            ENDIF()
            BREAK()
        ENDIF()
    ENDFOREACH()
        
    IF(NOT GFORTRAN_LIBRARY)
        MESSAGE(FATAL_ERROR "Could not retrieve location of GFortran Libraries from 'gcc -lgfortran'")
    ENDIF()

    GET_FILENAME_COMPONENT(GFORTLIB_FILE_NAME ${GFORTRAN_LIBRARY} NAME)
    SET(GFORTLIB_OUTPUT ${PYTHON_LIBS_DIR}/${GFORTLIB_FILE_NAME})

    ADD_CUSTOM_TARGET(
        ${GFORTLIB_NAME}
        DEPENDS ${GFORTLIB_OUTPUT}
    )

    ADD_CUSTOM_COMMAND(
        OUTPUT ${GFORTLIB_OUTPUT}
        COMMAND ${CMAKE_COMMAND} -E copy ${GFORTRAN_LIBRARY} ${GFORTLIB_OUTPUT}
    )

    ADD_DEPENDENCIES(${GFORTLIB_NAME} ${F2PY_NAME})

ENDMACRO(GET_GFORTRANLIBS)