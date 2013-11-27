# magics test executable run in script

list ( APPEND tests coast_attributes_ex  coast_boundaries  coast_res  coast_uk  coastonly_1  coastonly_2 landsea_shading)
foreach( test ${tests} )
    add_magics_test(NAME ${test} SOURCES ${test}.F IMAGE ${test}.ps THRESHOLD 50)
endforeach()

