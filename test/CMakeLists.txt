
# Basic test 
if ( ENABLE_PYTHON )
   ecbuild_add_test( TARGET basic_python
              TYPE PYTHON
              ARGS coast.py
              RESOURCES coast.py 
              ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
endif()

if ( ENABLE_FORTRAN) 
    ecbuild_add_test( TARGET basic_fortran_shared
            SOURCES   coast.f90
            LIBS      MagPlusSingle MagPlusShared
            LINKER_LANGUAGE Fortran )
    if ( ENABLE_STATIC_LIBRARY )
        ecbuild_add_test( TARGET basic_fortran_static
            SOURCES   coast.f90
            LIBS      MagPlusSingle MagPlusStatic
            LINKER_LANGUAGE Fortran )       
     endif()
endif()
    
if ( ENABLE_BUFR )
    if ( ENABLE_PYTHON )
       ecbuild_add_test( TARGET bufr_python
                  TYPE PYTHON
                  ARGS bufr.py
                  RESOURCES grib.py data.bufr 
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
    endif()
    if ( ENABLE_FORTRAN) 
        ecbuild_add_test( TARGET bufr_fortran
                SOURCES   bufr.f90
                LIBS      MagPlusSingle MagPlusShared
                RESOURCES data.bufr
                LINKER_LANGUAGE CXX )
        if ( ENABLE_STATIC_LIBRARY )
             ecbuild_add_test( TARGET bufr_fortran_static
                    SOURCES   bufr.f90
                    RESOURCES data.bufr
                    LIBS      MagPlusSingle MagPlusStatic
                    LINKER_LANGUAGE CXX )       
         endif()
    endif()
endif()




if ( ENABLE_GRIB )
    if ( ENABLE_PYTHON )
       ecbuild_add_test( TARGET grib_python
                  TYPE PYTHON
                  ARGS grib.py
                  RESOURCES grib.py data.grib
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
    endif()
    if ( ENABLE_FORTRAN) 
        ecbuild_add_test( TARGET grib_fortran
                SOURCES   grib.f90
                LIBS      MagPlusSingle MagPlusShared
                RESOURCES data.grib
                LINKER_LANGUAGE Fortran )
    endif()
endif()



if ( ENABLE_CAIRO ) 
    if ( ENABLE_PYTHON )
        ecbuild_add_test( TARGET cairo_python
                  TYPE PYTHON
                  ARGS cairo.py
                  RESOURCES cairo.py 
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
    endif()
      if ( ENABLE_FORTTAN )
        ecbuild_add_test( TARGET cairo_fortran
                SOURCES   cairo.f90
                LIBS      MagPlusSingle MagPlusShared
                LINKER_LANGUAGE Fortran )
    endif()
endif()

if ( ENABLE_ODB )
    if ( ENABLE_PYTHON )
        ecbuild_add_test( TARGET odb_python
                  TYPE PYTHON
                  ARGS odb.py
                  RESOURCES odb.py test.odb
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
    endif()
endif()

