ecbuild_add_test( TARGET python_magics
                  TYPE PYTHON
                  ARGS magics.py
                  RESOURCES magics.py
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )
