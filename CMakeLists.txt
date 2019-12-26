cmake_minimum_required(VERSION 3.14)
project(EZMPI Fortran)
add_library(ezmpi ezmpi.f90 ezmpi_recv.f90 ezmpi_send.f90 abort.F90)

set_target_properties(ezmpi PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)

target_include_directories(ezmpi PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)

option(USE_MPI "Use within MPI program" ON)

if(USE_MPI)
  find_package(MPI REQUIRED)
  target_link_libraries(ezmpi PUBLIC MPI::MPI_Fortran)
  target_compile_definitions(ezmpi PRIVATE HAVE_MPI)
endif()

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR})
include(ConfigureCompilerFlags)
