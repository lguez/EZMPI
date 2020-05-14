cmake_minimum_required(VERSION 3.14)
project(EZMPI Fortran)

add_library(ezmpi ezmpi.f90 ezmpi_recv.F90 ezmpi_send.F90 abort.F90
  ezmpi_bcast.F90)

set_target_properties(ezmpi PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)

target_include_directories(ezmpi PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)

option(EZMPI_USE_MPI "Use within MPI program" ON)

if(EZMPI_USE_MPI)
  find_package(MPI REQUIRED)
  target_link_libraries(ezmpi PUBLIC MPI::MPI_Fortran)
  target_compile_definitions(ezmpi PRIVATE HAVE_MPI)
endif()

set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR})
include(ConfigureCompilerFlags)

include(CMakePackageConfigHelpers)
configure_package_config_file(Config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
    INSTALL_DESTINATION lib/cmake/${PROJECT_NAME})

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG|Intel)
  set(install_include_dir include/${PROJECT_NAME})
  install(DIRECTORY ${PROJECT_BINARY_DIR}/modules/ DESTINATION
    ${install_include_dir})
else()
  set(install_include_dir include)
  install(FILES ${PROJECT_BINARY_DIR}/modules/ezmpi.mod TYPE INCLUDE)
endif()

install(TARGETS ezmpi EXPORT ${PROJECT_NAME}Targets LIBRARY
  INCLUDES DESTINATION ${install_include_dir})
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  DESTINATION lib/cmake/${PROJECT_NAME})
install(EXPORT ${PROJECT_NAME}Targets DESTINATION lib/cmake/${PROJECT_NAME})
export(EXPORT ${PROJECT_NAME}Targets)
