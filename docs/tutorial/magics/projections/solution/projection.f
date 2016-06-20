C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      program  magics

C open Magics
      call popen

C Define the name of the output 
      call psetc('output_name', 'projection')

C define the coordinates of the geographical area 
      call psetc('subpage_map_projection', 'cylindrical')
      call psetr('subpage_lower_left_longitude', -180.0)
      call psetr('subpage_upper_right_longitude', 180.0)
      call psetr('subpage_upper_right_latitude', 90.0)
      call psetr('subpage_lower_left_latitude', -90.0)

C define the coastlines attributes 
      call psetc('map_coastline_colour', 'grey')
      call psetc('map_coastline_land_shade', 'on')
      call psetc('map_coastline_sea_shade', 'on')
      call psetc('map_coastline_land_shade_colour', 'cream')
      call psetc('map_coastline_sea_shade_colour', 'white')
      call psetc('map_grid', 'on')
      call psetc('map_grid_colour', 'grey')
      call psetc('map_label', 'off')
	
C Call the action routine to plot the coastlines
      call pcoast

C define the text to plot 
      call psetc('text_line_1', 'Global Map!')
	  call pseti('text_line_count', 1)

C Alternative setting : using an array of string instead
c     call pset1c('text_lines', (/'Global Map!'/), 1)

      call ptext

      call pclose

      end
