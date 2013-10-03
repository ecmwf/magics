# magics test executable run in script

ecbuild_add_test( TARGET efas_graph
                  TYPE PYTHON
                  ARGS graph.py
                  RESOURCES graph.py
                  ENVIRONMENT PYTHONPATH=${MAG_PYTHON_PATH} )


