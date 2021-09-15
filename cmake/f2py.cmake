#   PROCESS f2py run
#   Author    :   K. Zarebski (UKAEA)
#   Date      :   last modified 2020-11-09
#
#   Run f2py on the given files list
MACRO(F2PY)
    EXECUTE_PROCESS (
        COMMAND bash -c "${PYTHON_EXECUTABLE} -c \"import sysconfig; print(sysconfig.get_config_var('EXT_SUFFIX'))\""
        OUTPUT_VARIABLE CMAKE_PYTHON_ABI_VERSION
    )
    STRING(STRIP ${CMAKE_PYTHON_ABI_VERSION} CMAKE_PYTHON_ABI_VERSION)

    SET(F2PY_TARGET ${CMAKE_BINARY_DIR}/fortran${CMAKE_PYTHON_ABI_VERSION})
    SET(F2PY_OUTPUT ${PYTHON_MODULE_DIR}/fortran${CMAKE_PYTHON_ABI_VERSION})
    LIST(TRANSFORM PROCESS_SRCS PREPEND ${PROCESS_SRC_DIR}/ OUTPUT_VARIABLE PROCESS_WRAP_SRC_PATHS)
    LIST(APPEND PROCESS_WRAP_SRC_PATHS ${PPED_F2PY_SRC})
    SET(F2PY_NAME "f2py")
    MESSAGE(STATUS "[f2py]: ")
    MESSAGE(STATUS "\tTarget: ${F2PY_TARGET}")
    MESSAGE(STATUS "\tf2py sources: ${PROCESS_WRAP_SRC_PATHS} ")
    ADD_CUSTOM_TARGET(
        ${F2PY_NAME}
        DEPENDS ${F2PY_TARGET} ${F2PY_OUTPUT} 
    )
    IF(NOT CMAKE_HOST_APPLE)
        ADD_CUSTOM_COMMAND(
            OUTPUT ${F2PY_TARGET} ${F2PY_OUTPUT}
            COMMAND echo \"Running f2py:\"\; LDFLAGS=-Wl,-rpath=\\$$ORIGIN/lib ${F2PY_NAME} -c -L../process/lib/ -l${PROJECT_NAME} ${PROCESS_WRAP_SRC_PATHS} --build-dir ${CMAKE_BINARY_DIR} -m "fortran"
            COMMAND ${CMAKE_COMMAND} -E copy ${F2PY_TARGET} ${F2PY_OUTPUT}
        )
    ELSE()
        ADD_CUSTOM_COMMAND(
            OUTPUT ${F2PY_TARGET} ${F2PY_OUTPUT}
            COMMAND echo "Running f2py to produce target '${F2PY_TARGET}':"
            COMMAND ${F2PY_NAME}-${F90WRAP_NAME} -c  -L../process/lib -l${PROJECT_NAME} ${PROCESS_SRCS} --build-dir ${CMAKE_BINARY_DIR}
            COMMAND ${CMAKE_COMMAND} -E copy ${F2PY_TARGET} ${F2PY_OUTPUT}
        )
    ENDIF()

    ADD_DEPENDENCIES(${F2PY_NAME} ${PIP_NAME} ${PROJECT_NAME} ${PREPROCESS_NAME})
ENDMACRO()