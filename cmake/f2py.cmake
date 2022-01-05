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
    SET(F2PY_NAME "f2py")
    MESSAGE(STATUS "[f2py]: ")
    MESSAGE(STATUS "\tTarget: ${F2PY_TARGET}")
    ADD_CUSTOM_TARGET(
        ${F2PY_NAME}
        DEPENDS ${F2PY_TARGET} ${F2PY_OUTPUT}
    )
    ADD_CUSTOM_COMMAND(
        OUTPUT ${F2PY_TARGET} ${F2PY_OUTPUT}
        COMMAND echo \"Running f2py:\"\; LDFLAGS=-Wl,-rpath=\\$$ORIGIN/lib ${F2PY_NAME} -c -L../process/lib/ -l${PROJECT_NAME} ${PREPROCESSED_SOURCE_FILES_PATH} --build-dir ${CMAKE_BINARY_DIR} -m fortran
        COMMAND ${CMAKE_COMMAND} -E copy ${F2PY_TARGET} ${F2PY_OUTPUT}
        DEPENDS ${PREPROCESS_TARGET_NAMES} ${PREPROCESSED_SOURCE_FILES_PATH} # rerun the wrapping when any of the preprocessed source files change
    )

    ADD_DEPENDENCIES(${F2PY_NAME} ${PIP_NAME} ${PROJECT_NAME})
ENDMACRO()