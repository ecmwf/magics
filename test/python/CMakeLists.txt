configure_file( magics.py magics.py )

set( MAG_PYTHON_PATH ${CMAKE_BINARY_DIR}/python )

ecbuild_add_test ( TARGET python_magics 
                   TYPE PYTHON 
                   ARGS magics.py 
                   ENVIRONMENT PYTHON_PATH=${MAG_PYTHON_PATH} )
                   
# FIXME: we should use RESOURCES magics.py, but isn't working becasue this test does not build a target.
