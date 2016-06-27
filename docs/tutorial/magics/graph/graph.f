C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program  magics
            
C define the data for the boxplot
      real pos(3), min(3), max(3)
      real upper(3), lower(3), median(3)
    
C define the data for the graph
      real x(3), y(3)
C define the text   
      character*10 lines(1)

      data pos /-25.,0.,25./
      data min /5.,10.,7./
      data max /95.,80.,92./
      data upper / 80.,72.,81./
      data lower / 12.,18.,30./
      data median / 45.,52.,47./
      data x /-25.,0.,25 /
      data y /5.,25.,75./
      data lines /"My Graph"/

C open magics
      call popen

C Setting of the output file name
      call ???

C define the cartesian projection
      call psetc("subpage_map_projection", "???") 
      call psetr("subpage_y_length", 14.)
      call psetr("subpage_y_position", 1.5)

C define horizontal axis
      call psetc("axis_orientation","horizontal")
      call psetc("axis_grid", "on")
	 ???
      call psetr("??min??", -50.)
      call psetr("??max??", 50.)
      call paxis

C define horizontal axis
      call psetc("axis_orientation", "???")
      call psetc("axis_grid", "on")
      call psetc("axis_grid_colour", "grey")
      call pseti("axis_grid_thickness", 1)
      call psetc("axis_grid_line_style", "dot")
      call psetr("??min??", 0.)
      call psetr("??min??", 100.) 
      call p???

C define the BoxPlots
      call pset1r("boxplot_positions", pos, 3)
      call pset1r("???minimum_values", min, 3)
      call ???("???maximum_values", max,3)
      call pset1r("???plot_box_upper_values", upper, 3)
      call ???("???_box_lower_values", lower, 3)
      call pset1r("???_median_values", median, 3)
      call pboxplot

C Define the graph 
      call psetc("graph_line_colour", "red")
      call pseti("graph_line_thickness", 8)
      call psetc("graph_symbol", "on")
      call psetc("legend_user_text", 
     + "<font colour='red'> My red curve </font>")
      call pseti("graph_symbol_marker_index", 1)
      call psetr("graph_symbol_height", 0.5)
      call p???("graph_curve_x_values", x, 3)
      call p???("graph_curve_y_values", y, 3)
      call p??? 

C Define the text
      call pset1c("text_lines", lines, 1)
      call psetc("text_html", ?true? )
      call psetc("text_colour", "black")
      call psetr("text_font_size", 0.6)
      call psetc("text_mode", "positional")
      ????
      call psetc("text_border", "off")
      call psetc("text_justification", "left")
      call ptext
    
      call psetc("legend_text_colour", "black")

      call p???
    
      end
