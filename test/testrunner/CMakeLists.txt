
## use multithreaded boost libraries, with -mt suffix
set(Boost_USE_MULTITHREADED ON)
option(Boost_USE_STATIC_LIBS "Link Boost statically" ON)
if (WIN32)
    option(Boost_USE_STATIC_RUNTIME "Link Boost against static C++ runtime libraries" ON)
endif()

set(BOOST_COMPONENTS "filesystem;unit_test_framework;program_options;system")

find_package(Boost CONFIG REQUIRED)


add_subdirectory(evmc)

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
target_link_libraries(testrunner evmc Boost::headers nlohmann_json::nlohmann_json)


