# magics test executable run in script

ecbuild_add_magics_test(NAME macc_geomap 
                SOURCES geomap.json 
                RESOURCES geodata.json 
                IMAGE geomap.png 
                THRESHOLD 10 
                JSON)

