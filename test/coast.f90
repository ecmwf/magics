	program magics

	call popen

	call pset1c("output_formats", (/"ps"/), 1)
	call psetc("output_name", "coast")
	call psetc("output_name_first_page_number", "off")

	call psetc("subpage_map_projection", "cylindrical")

	call psetc("map_grid_colour", "tan")
	call psetc("map_grid", "on")
	call psetc("map_coastline_colour", "tan")
	call pcoast

	call preset('map_grid_colour')
	call preset('map_grid')
	call preset('map_coastline_colour')

	call pclose

	end
