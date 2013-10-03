# magics test executable run in script



ecbuild_add_magics_test(NAME macc_geomap 
                SOURCES geomap.json 
                RESOURCES geodata.json 
                IMAGE geomap.png 
                THRESHOLD 10 
                JSON)

ecbuild_add_magics_test(NAME macc_graph 
                SOURCES graph.json 
                RESOURCES xydata.json 
                IMAGE macc_graph.png 
                THRESHOLD 10 
                JSON)

ecbuild_add_magics_test(NAME macc_symb 
                SOURCES symb.json 
                RESOURCES xydata.json 
                IMAGE macc_symb.png 
                THRESHOLD 10 
                JSON)
