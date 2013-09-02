
	
	program magics

	call popen

	call psetc("output_name", "tephi")
	call psetc("subpage_map_projection", "tephigram")

	call ptephi

	call pclose

	end
