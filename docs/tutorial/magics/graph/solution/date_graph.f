      program  magics
      
      
C define the data for the 2 graphs
      real ymin(3), ymax(3)
      character*22 x(3)

C define the text   
      character*20 lines(1)
    
    
      data ymin /12.,17.,15./
      data ymax /22.,21.,17./
     
      data x /"2011-03-02 00:00:00",
     +        "2011-03-02 12:00:00",
     +        "2011-03-03 00:00:00"/

      data lines /"My other Graph"/



C open magics
      call popen

C Setting of the output file name
      call psetc('output_name', 'date_graph')



C define the cartesian projection
      call psetc("subpage_map_projection", "cartesian") 
	  call psetr("subpage_y_length", 14.)
	  call psetr("subpage_y_position", 1.5)



C define horizontal axis
      call psetc("axis_orientation","horizontal")
      call psetc("axis_type","date")
      call psetc("axis_grid", "on")
	  call psetc("axis_grid_colour", "grey")
	  call pseti("axis_grid_thickness", 1)
      call psetc("axis_date_min_value", "2011-03-01 12:00:00")
      call psetc("axis_date_max_value", "2011-03-03 12:00:00")
	  call psetc("axis_grid_line_style", "dot")
      call paxis

C define horizontal axis
      call psetc("axis_orientation", "vertical") 
      call psetc("axis_type","regular")
      call psetc("axis_grid", "on")
      call psetc("axis_grid_colour", "grey")
      call pseti("axis_grid_thickness", 1)
      call psetc("axis_grid_line_style", "dot")
      call psetr("axis_min_value", 10.)
      call psetr("axis_max_value", 30.) 
      call paxis

C Set the Legend
      call psetc("legend", "on")
      call psetc("legend_text_colour", "black")

C Define the blue gurve with the min values
      call psetc("graph_line_colour", "blue")
      call pseti("graph_line_thickness", 8)
      call psetc("graph_symbol", "on")
      call psetc("legend_user_text", 
     + "<font colour='blue'> Min </font>")
      call pseti("graph_symbol_marker_index", 1)
      call psetr("graph_symbol_height", 0.5)
      call psetc("graph_symbol_colour", "black")
      call pset1c("graph_curve_date_x_values", x, 3)
	  call pset1r("graph_curve_y_values", ymin, 3)
      call pgraph 


C Define the blue gurve with the max values
      call psetc("graph_line_colour", "red")
      call pseti("graph_line_thickness", 8)
      call psetc("graph_symbol", "on")
      call psetc("legend_user_text", 
     + "<font colour='red'> Max </font>")
      call pseti("graph_symbol_marker_index", 1)
      call psetr("graph_symbol_height", 0.5)
      call psetc("graph_symbol_colour", "black")
      call pset1c("graph_curve_date_x_values", x, 3)
	  call pset1r("graph_curve_y_values", ymax, 3)
      call pgraph 
    


C Define the text
      call pset1c("text_lines", lines, 1)
      call psetc("text_html", "true")
      call psetc("text_colour", "black")
      call psetr("text_font_size", 0.6)
      call psetc("text_mode", "positional")
      call psetr("text_box_x_position", 1.5)
      call psetr("text_box_y_position", 16.5)
      call psetr("text_box_x_length", 20.)
      call psetr("text_box_y_length", 2.5)
      call psetc("text_border", "off")
      call psetc("text_justification", "left")
      call ptext
    

      call pclose
    
      end












