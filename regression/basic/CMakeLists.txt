# magics test executable run in script


ecbuild_add_magics_test(NAME basic SOURCES basic.c IMAGE basic.png DATA data.grib ) 
ecbuild_add_magics_test(NAME contour SOURCES contour.f IMAGE contour.ps DATA z500.grb THRESHOLD 50) 
ecbuild_add_magics_test(NAME symbol SOURCES symbol.py IMAGE symbol.png THRESHOLD 30 PYTHON) 


