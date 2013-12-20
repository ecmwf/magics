	program magics

	call popen

	call pset1c("output_formats", (/"ps"/), 1)
	call psetc("output_name", "grib")
	call psetc("output_name_first_page_number", "off")

	call psetr("subpage_lower_left_longitude", -20.00)
	call psetr("subpage_upper_right_longitude", 60.00)
	call psetr("subpage_upper_right_latitude", 60.00)
	call psetc("subpage_map_projection", "cylindrical")
	call psetr("subpage_lower_left_latitude", 30.00)

	call psetc("grib_id", "data")
	call psetc("grib_input_file_name", "data.grib")
	call pgrib

	call preset('grib_id')
	call preset('grib_input_file_name')

	call pcont


	call psetc("map_grid_colour", "tan")
	call psetc("map_grid", "on")
	call psetc("map_coastline_colour", "tan")
	call pcoast
	call ptext

	call preset('map_grid_colour')
	call preset('map_grid')
	call preset('map_coastline_colour')

	call pclose

	end
