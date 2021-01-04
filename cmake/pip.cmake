#   Check all Pip modules exist
#   Author  :   K. Zarebski
#   Date    : last modified 2020-11-05
#
#   Checks to see if all Python module requirements are satisfied else
#   runs python pip on the requirements file

MACRO(PIP_INSTALL)
    SET(PIP_NAME "pip_installs")

    include(ExternalProject)
    ExternalProject_Add(
        ford_git
        GIT_REPOSITORY https://github.com/jonMaddockUkaea/ford.git
        UPDATE_COMMAND ""
        INSTALL_COMMAND bash -c "${PYTHON_EXECUTABLE} -m pip install ."
        BUILD_COMMAND ""
        CONFIGURE_COMMAND ""
        BUILD_IN_SOURCE 1
        BYPRODUCTS ${CMAKE_BINARY_DIR}/ford_git-prefix/
    )

    SET(MODULE_REQUIREMENTS_FILE ${CMAKE_SOURCE_DIR}/requirements.txt)
    STRING(REPLACE "/" "_" PIP_OUT_PREFIX ${PYTHON_EXECUTABLE})
    SET(PIP_COMPLETE_FILE ${CMAKE_BINARY_DIR}/${PIP_OUT_PREFIX}.touch)
    ADD_CUSTOM_TARGET(
        ${PIP_NAME}
        DEPENDS ${PIP_COMPLETE_FILE}
    )

    ADD_CUSTOM_COMMAND(
        OUTPUT ${PIP_COMPLETE_FILE}
        COMMAND ${PYTHON_EXECUTABLE} -m pip install -r ${MODULE_REQUIREMENTS_FILE}
        COMMAND touch ${PIP_COMPLETE_FILE}
    )

    ADD_DEPENDENCIES(${PIP_NAME} ford_git)

ENDMACRO()