! (C) Copyright 1996-2016 ECMWF.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
! In applying this licence, ECMWF does not waive the privileges and immunities 
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.
!

	program magics

	call popen

	call pset1c("output_formats", (/"ps"/), 1)
	call psetc("output_name", "bufr")
	call psetc("output_name_first_page_number", "off")

	call psetr("subpage_lower_left_longitude", -20.00)
	call psetr("subpage_upper_right_longitude", 60.00)
	call psetr("subpage_upper_right_latitude", 60.00)
	call psetc("subpage_map_projection", "cylindrical")
	call psetr("subpage_lower_left_latitude", 30.00)

	call psetc("obs_input_file_name", "synop.bufr")
	call pobs

	call preset('obs_input_file_name')

	call psetc("map_grid_colour", "tan")
	call psetc("map_grid", "on")
	call psetc("map_coastline_colour", "tan")
	call pcoast

	call preset('map_grid_colour')
	call preset('map_grid')
	call preset('map_coastline_colour')

	call pclose

	end
