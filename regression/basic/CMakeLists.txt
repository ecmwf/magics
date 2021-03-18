# magics test executable run in script

add_magics_test(NAME basic SOURCES basic.c IMAGE basic.png DATA data.grib ) 
add_magics_test(NAME contour SOURCES contour.f IMAGE contour.ps DATA z500.grb THRESHOLD 50)
add_magics_test(NAME graph SOURCES graph.json RESOURCES data.json IMAGE graph.png THRESHOLD 10 JSON)

