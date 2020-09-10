set( eccodes_environment
  ECCODES_DEFINITION_PATH=${eccodes_BASE_DIR}/share/${grib_handling_pkg}/definitions
  ECCODES_SAMPLES_PATH=${eccodes_BASE_DIR}/share/${grib_handling_pkg}/samples
  )

# Basic test
ecbuild_add_test( TARGET          coast_c
                  SOURCES         coast.c
                  LIBS            MagPlusSingle MagPlus
                  ENVIRONMENT     MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE C )

ecbuild_add_test( TARGET          contour_c
                  SOURCES         contour.c
                  CONDITION       HAVE_GRIB
                  RESOURCES       data.grib
                  LIBS            MagPlusSingle MagPlus
                  ENVIRONMENT     MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                                   ${eccodes_environment}
                  LINKER_LANGUAGE C )

ecbuild_add_test( TARGET          netcdf_c
                  SOURCES         netcdf.c
                  CONDITION       HAVE_NETCDF
                  RESOURCES       era5_2mt.nc
                  LIBS            MagPlusSingle MagPlus
                  ENVIRONMENT     MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE C )

