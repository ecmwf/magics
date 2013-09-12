set( MAG_PYTHON_PATH ${CMAKE_BINARY_DIR}/python )

ecbuild_add_test( TARGET python_magics
                  TYPE PYTHON
                  ARGS magics.py
                  RESOURCES magics.py
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
