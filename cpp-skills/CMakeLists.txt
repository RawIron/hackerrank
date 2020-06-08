cmake_minimum_required(VERSION 3.3)

project("cppskills")

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Weffc++ -Wextra -Wsign-conversion -Werror")

find_package(Boost COMPONENTS REQUIRED unit_test_framework)
IF (Boost_FOUND)
    include_directories(${Boost_INCLUDE_DIR})
endif()


set(test_SOURCES
    run_tests.cpp
   )

add_executable(run_tests ${test_SOURCES})
target_link_libraries(run_tests boost_unit_test_framework)

add_executable(forloop intro_forloop.cpp)
add_executable(functions intro_functions.cpp)
add_executable(pointer intro_pointer.cpp)
add_executable(arrays intro_arrays.cpp)
add_executable(inherited classes_inherited.cpp)
add_executable(vectorerase stl_vectorerase.cpp)
add_executable(lowerbound stl_lowerbound.cpp)
add_executable(sets stl_sets.cpp)
add_executable(maps stl_maps.cpp)
add_executable(deque stl_deque.cpp)
