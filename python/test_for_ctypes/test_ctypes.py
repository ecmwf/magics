import magics as ma

ma.init()
ma.setr("subpage_lower_left_latitude",   40.)
ma.setr("subpage_lower_left_longitude", -20.)
ma.setr("subpage_upper_right_latitude",  65.)
ma.setr("subpage_upper_right_longitude", 10.)

ma.setc("grib_input_file_name", "../test/data.grib")
ma.grib()

ma.cont()

ma.coast()

#print ma.enqr("subpage_lower_left_latitude")

ma.new_page("page")

ma.reset("subpage_lower_left_latitude")
ma.reset("subpage_lower_left_longitude")

ma.coast()
#print ma.enqr("subpage_lower_left_latitude")

ma.info()

ma.finalize()
