cmake_minimum_required(VERSION 3.3)

project("cppskills")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Weffc++ -Wextra -Wsign-conversion -Werror")

find_package(Boost COMPONENTS REQUIRED unit_test_framework)
IF (Boost_FOUND)
    include_directories(${Boost_INCLUDE_DIR})
endif()


set(cppskills_SOURCES
    run_tests.cpp
    )

add_executable(run_tests ${cppskills_SOURCES})
target_link_libraries(run_tests boost_unit_test_framework)

add_executable(forloop intro_forloop.cpp)
