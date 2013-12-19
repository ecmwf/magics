
	program magics

	call popen

	call pcoast
	call psetc("grib_input_file_name", "data.grib")

	call pgrib

	call pcont
	call ptext

	call pclose

	end
