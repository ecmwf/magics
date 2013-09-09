configure_file( magics.magml magics.magml )





ecbuild_add_test ( TARGET magics_json 
                   TYPE SCRIPT
                   COMMAND ${CMAKE_BINARY_DIR}/bin/magmlx
                   ARGS magics.magml
                  )
                   
# FIXME: we should use RESOURCES magics.py, but isn't working becasue this test does not build a target.
