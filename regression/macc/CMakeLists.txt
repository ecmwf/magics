# magics test executable run in script



add_magics_test(NAME macc_geomap
                SOURCES geomap.json
                RESOURCES geodata.json
                IMAGE geomap.png
                THRESHOLD 1
                JSON)

add_magics_test(NAME macc_graph
                SOURCES graph.json
                RESOURCES xydata.json
                IMAGE macc_graph.png
                THRESHOLD 1
                JSON)

add_magics_test(NAME macc_symb
                SOURCES symb.json
                RESOURCES xydata.json
                IMAGE macc_symb.png
                THRESHOLD 1
                JSON)

add_magics_test(NAME macc_taylor
                SOURCES taylor.json
                RESOURCES taylordata.json
                IMAGE macc_taylor.png
                THRESHOLD 1
                JSON)
add_magics_test(NAME macc_diagonal
                SOURCES diagonal.json
                RESOURCES diagonal.data
                IMAGE diagonal.png
                THRESHOLD 1
                JSON)

