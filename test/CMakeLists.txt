set( eccodes_environment
  ECCODES_DEFINITION_PATH=${eccodes_BASE_DIR}/share/${grib_handling_pkg}/definitions
  ECCODES_SAMPLES_PATH=${eccodes_BASE_DIR}/share/${grib_handling_pkg}/samples )

# Basic test
ecbuild_add_test( TARGET basic_c
                  SOURCES   coast.c
                  LIBS      MagPlusSingle MagPlus
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE C )

ecbuild_add_test( TARGET contour_c
                  SOURCES   contour.c
                  CONDITION HAVE_GRIB
                  LIBS      MagPlusSingle MagPlus
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                              ${eccodes_environment}
                  LINKER_LANGUAGE C )


ecbuild_add_test( TARGET basic_fortran_shared
                  SOURCES   coast.f90
                  LIBS      MagPlusSingle MagPlus
                  CONDITION HAVE_FORTRAN
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET basic_fortran_static
                  SOURCES   coast.f90
                  LIBS      MagPlusSingleStatic MagPlusStatic ${MAGICS_EXTRA_LIBRARIES}
                  CONDITION HAVE_FORTRAN AND BUILD_SHARED_LIBS STREQUAL "BOTH"
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE Fortran )






ecbuild_add_test( TARGET bufr_fortran
                  SOURCES   bufr.f90
                  LIBS      MagPlusSingle MagPlus
                  CONDITION HAVE_BUFR AND HAVE_FORTRAN
                  RESOURCES synop.bufr
                  LINKER_LANGUAGE Fortran
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                              ${eccodes_environment})

ecbuild_add_test( TARGET bufr_fortran_static
                  SOURCES   bufr.f90
                  RESOURCES synop.bufr
                  LIBS      MagPlusSingleStatic MagPlusStatic
                  CONDITION HAVE_BUFR AND HAVE_FORTRAN AND BUILD_SHARED_LIBS STREQUAL "BOTH"
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE Fortran )



ecbuild_add_test( TARGET grib_fortran
                  SOURCES   grib.f90
                  LIBS      MagPlusSingle MagPlus
                  RESOURCES data.grib
                  CONDITION HAVE_GRIB AND HAVE_FORTRAN
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  ${grib_environment}
                  LINKER_LANGUAGE Fortran
                  )



ecbuild_add_test( TARGET cairo_fortran
                  SOURCES   cairo.f90
                  LIBS      MagPlusSingle MagPlus
                  CONDITION HAVE_CAIRO AND HAVE_FORTRAN
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE Fortran )


