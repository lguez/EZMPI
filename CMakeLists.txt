cmake_minimum_required(VERSION 3.16)
list(APPEND CMAKE_MESSAGE_CONTEXT EZMPI)
project(EZMPI LANGUAGES Fortran)
add_library(ezmpi ezmpi.f90 ezmpi_recv.f90 ezmpi_ssend.f90
  ezmpi_send.f90 ezmpi_bcast.f90)
set_target_properties(ezmpi PROPERTIES Fortran_MODULE_DIRECTORY
  ${PROJECT_BINARY_DIR}/modules)
target_include_directories(ezmpi PUBLIC
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/modules>)
find_package(MPI REQUIRED)
  
if(NOT MPI_Fortran_HAVE_F08_MODULE)
  message(FATAL_ERROR "mpi_f08 is not available.")
endif()

target_link_libraries(ezmpi PRIVATE MPI::MPI_Fortran)
add_library(${PROJECT_NAME}::ezmpi_util ALIAS ezmpi)

# Start installation part

if(CMAKE_Fortran_COMPILER_ID MATCHES NAG|Intel|PGI)
  set(install_include_dir include/${PROJECT_NAME})
  install(DIRECTORY ${PROJECT_BINARY_DIR}/modules/ DESTINATION
    ${install_include_dir})
else()
  set(install_include_dir include)
  install(FILES ${PROJECT_BINARY_DIR}/modules/ezmpi.mod TYPE INCLUDE)
endif()

install(TARGETS ezmpi EXPORT ${PROJECT_NAME}Targets INCLUDES
  DESTINATION ${install_include_dir})
install(EXPORT ${PROJECT_NAME}Targets DESTINATION
  lib/cmake/${PROJECT_NAME} NAMESPACE ${PROJECT_NAME}:: FILE
  ${PROJECT_NAME}Config.cmake)

# End installation part

export(EXPORT ${PROJECT_NAME}Targets NAMESPACE ${PROJECT_NAME}:: FILE
  ${PROJECT_NAME}Config.cmake)
