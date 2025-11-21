cmake_minimum_required(VERSION 3.23)
project(TestRunner)

## use multithreaded boost libraries, with -mt suffix
set(Boost_USE_MULTITHREADED ON)
option(Boost_USE_STATIC_LIBS "Link Boost statically" ON)
if (WIN32)
    option(Boost_USE_STATIC_RUNTIME "Link Boost against static C++ runtime libraries" ON)
endif()

set(BOOST_COMPONENTS "filesystem;unit_test_framework;program_options;system")

find_package(Boost CONFIG REQUIRED)

include_directories(AFTER SYSTEM .)
add_subdirectory(evmc)
set_property(TARGET evmc PROPERTY CXX_STANDARD 20)
add_executable(
        testrunner
        common.h
        EVMHost.cpp
        EVMHost.h
        EVMVersion.cpp
        EVMVersion.h
        Exceptions.cpp
        Exceptions.h
        FixedHash.h
        Instruction.cpp
        Instruction.h
        Keccak256.cpp
        Keccak256.h
        picosha2.h
        testrunner.cpp
)
set_property(TARGET testrunner PROPERTY CXX_STANDARD 20)


include(FetchContent)

FetchContent_Declare(json URL https://github.com/nlohmann/json/releases/download/v3.12.0/json.tar.xz)
FetchContent_MakeAvailable(json)

target_link_libraries(testrunner evmc Boost::headers nlohmann_json::nlohmann_json)


