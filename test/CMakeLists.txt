set( eccodes_environment
  ECCODES_DEFINITION_PATH=${eccodes_BASE_DIR}/share/${grib_handling_pkg}/definitions
  ECCODES_SAMPLES_PATH=${eccodes_BASE_DIR}/share/${grib_handling_pkg}/samples
  )

# Basic test
ecbuild_add_test( TARGET basic_c
                  SOURCES   coast.c
                  LIBS      MagPlusSingle MagPlus
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE C )

ecbuild_add_test( TARGET contour_c
                  SOURCES   contour.c
                  CONDITION HAVE_GRIB
                  RESOURCES data.grib
                  LIBS      MagPlusSingle MagPlus
                  RESOURCES data.grib
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                              ${eccodes_environment}
                  LINKER_LANGUAGE C )








