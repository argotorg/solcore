cmake_minimum_required(VERSION 3.13)
project(solcore VERSION 0.0 LANGUAGES C CXX)

option(IGNORE_VENDORED_DEPENDENCIES "Ignore libraries provided as submodules of the repository" OFF)
mark_as_advanced(IGNORE_VENDORED_DEPENDENCIES)

if (NOT IGNORE_VENDORED_DEPENDENCIES)
    add_subdirectory(
        ${CMAKE_SOURCE_DIR}/deps/nlohmann_json
        EXCLUDE_FROM_ALL
    )
else()
    find_package(nlohmann_json REQUIRED)
endif()

set(CMAKE_CXX_STANDARD 23)
add_subdirectory(test/testrunner)
