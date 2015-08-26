# Basic test 
ecbuild_add_test( TARGET basic_c
                  SOURCES   coast.c
                  LIBS      MagPlusSingleShared MagPlusShared
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE C )

ecbuild_add_test( TARGET basic_python
                  TYPE PYTHON
                  CONDITION HAVE_PYTHON
                  ARGS coast.py
                  RESOURCES coast.py 
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/.. )

ecbuild_add_test( TARGET basic_fortran_shared
                  SOURCES   coast.f90
                  LIBS      MagPlusSingleShared MagPlusShared
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
                  RESOURCES bufr.py data.bufr 
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/.. )

ecbuild_add_test( TARGET bufr_fortran
                  SOURCES   bufr.f90
                  LIBS      MagPlusSingleShared MagPlusShared
                  CONDITION HAVE_BUFR AND HAVE_FORTRAN
                  RESOURCES data.bufr
                  LINKER_LANGUAGE Fortran 
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..)

ecbuild_add_test( TARGET bufr_fortran_static
                  SOURCES   bufr.f90
                  RESOURCES data.bufr
                  LIBS      MagPlusSingleStatic MagPlusStatic
                  CONDITION HAVE_BUFR AND HAVE_FORTRAN AND BUILD_SHARED_LIBS STREQUAL "BOTH"
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE Fortran )       

ecbuild_add_test( TARGET grib_python
                  TYPE PYTHON
                  CONDITION HAVE_GRIB AND HAVE_PYTHON
                  ARGS grib.py
                  RESOURCES grib.py data.grib
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/.. )

ecbuild_add_test( TARGET grib_fortran
                  SOURCES   grib.f90
                  LIBS      MagPlusSingleShared MagPlusShared
                  RESOURCES data.grib
                  CONDITION HAVE_GRIB AND HAVE_FORTRAN
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET cairo_python
                  TYPE PYTHON
                  CONDITION HAVE_CAIRO AND HAVE_PYTHON
                  ARGS cairo.py
                  RESOURCES cairo.py 
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/.. )

ecbuild_add_test( TARGET cairo_fortran
                  SOURCES   cairo.f90
                  LIBS      MagPlusSingleShared MagPlusShared
                  CONDITION HAVE_CAIRO AND HAVE_FORTRAN
                  ENVIRONMENT MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/..
                  LINKER_LANGUAGE Fortran )

ecbuild_add_test( TARGET odb_python
                  TYPE PYTHON
                  CONDITION HAVE_ODB AND HAVE_PYTHON
                  ARGS odb.py
                  RESOURCES odb.py test.odb
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH}
                              LD_LIBRARY_PATH=${CMAKE_BINARY_DIR}/lib
                              MAGPLUS_HOME=${CMAKE_CURRENT_SOURCE_DIR}/.. )
