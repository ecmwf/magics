set( grib_environment
    GRIB_DEFINITION_PATH=${grib_api_BASE_DIR}/share/${grib_handling_pkg}/definitions
    GRIB_SAMPLES_PATH=${grib_api_BASE_DIR}/share/${grib_handling_pkg}/samples )

# Basic test
ecbuild_add_test( TARGET basic_c
                  SOURCES   coast.c
                  LIBS      MagPlusSingle MagPlus
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE C )

ecbuild_add_test( TARGET contour_c
                  SOURCES   contour.c
                  LIBS      MagPlusSingle MagPlus
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE C )

ecbuild_add_test( TARGET basic_python
                  TYPE PYTHON
                  CONDITION HAVE_PYTHON
                  ARGS coast.py
                  RESOURCES coast.py
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              DYLD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/.. )

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

ecbuild_add_test( TARGET bufr_python
                  TYPE PYTHON
                  CONDITION HAVE_BUFR AND HAVE_PYTHON
                  ARGS bufr.py
                  RESOURCES bufr.py synop.bufr
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              DYLD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/.. )

ecbuild_add_test( TARGET bufr_fortran
                  SOURCES   bufr.f90
                  LIBS      MagPlusSingle MagPlus
                  CONDITION HAVE_BUFR AND HAVE_FORTRAN
                  RESOURCES synop.bufr
                  LINKER_LANGUAGE Fortran
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..)

ecbuild_add_test( TARGET bufr_fortran_static
                  SOURCES   bufr.f90
                  RESOURCES synop.bufr
                  LIBS      MagPlusSingleStatic MagPlusStatic
                  CONDITION HAVE_BUFR AND HAVE_FORTRAN AND BUILD_SHARED_LIBS STREQUAL "BOTH"
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET grib_python
                  TYPE PYTHON
                  CONDITION HAVE_CAIRO AND HAVE_GRIB AND HAVE_PYTHON
                  ARGS grib.py
                  RESOURCES grib.py data.grib
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              DYLD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                              ${grib_environment} )

ecbuild_add_test( TARGET grib_fortran
                  SOURCES   grib.f90
                  LIBS      MagPlusSingle MagPlus
                  RESOURCES data.grib
                  CONDITION HAVE_GRIB AND HAVE_FORTRAN
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  ${grib_environment}
                  LINKER_LANGUAGE Fortran
                  )

ecbuild_add_test( TARGET cairo_python
                  TYPE PYTHON
                  CONDITION HAVE_CAIRO AND HAVE_PYTHON
                  ARGS cairo.py
                  RESOURCES cairo.py
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              DYLD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/.. )

ecbuild_add_test( TARGET cairo_fortran
                  SOURCES   cairo.f90
                  LIBS      MagPlusSingle MagPlus
                  CONDITION HAVE_CAIRO AND HAVE_FORTRAN
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET odb_python
                  TYPE PYTHON
                  CONDITION HAVE_CAIRO AND HAVE_ODB AND HAVE_PYTHON
                  ARGS odb.py
                  RESOURCES odb.py test.odb
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              DYLD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/.. )
