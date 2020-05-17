cmake_minimum_required(VERSION 3.3)

project("daysofcode")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Weffc++ -Wextra -Wsign-conversion -Werror")

find_package(Boost COMPONENTS REQUIRED unit_test_framework)
IF (Boost_FOUND)
    include_directories(${Boost_INCLUDE_DIR})
endif()


set(daysofcode_SOURCES
    run_tests.cc
    14_scope.cc)

add_executable(run_tests ${daysofcode_SOURCES})
target_link_libraries(run_tests boost_unit_test_framework)

add_executable(loops 5_loops.cc)
add_executable(scope 14_scope.cc)
add_executable(binary 10_binary.cc)
add_executable(arrays2d 11_arrays2d.cc)
add_executable(inheritance 12_inheritance.cc)
add_executable(linkedlist 15_linkedlist.cc)
add_executable(exceptions1 16_exceptions1.cc)
add_executable(exceptions2 17_exceptions2.cc)
