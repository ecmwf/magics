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
      call psetc('output_name', 'multi_projection')


      call area('Global Cylindrical', 'cylindrical        ', 
     + -90., -180., 90., 180., 0., 'north')
	  call area('Europe            ', 'polar_stereographic', 
     + 21.51, -37.27, 51.28, 65., 0., 'north')
      call area('North America     ', 'polar_stereographic', 
     + -5.0, -140.0, 30.0, -15.0, -100., 'north')
      call area('South America     ', 'cylindrical        ', 
     + -65.0, -125.0, 20.0, 5.0, 0., 'north')
      call area('Asia              ', 'cylindrical        ', 
     + 0.00, 55.00, 80.00, 175.00, 0., 'north')
      call area('Australia         ', 'cylindrical        ', 
     + -55.00, 80.0, -5.0, 190.0, 0., 'north')
      call area('Africa            ', 'cylindrical        ', 
     + -40.0, -45.00, 40.00, 75.00, 0., 'north')

      call pclose

      end 
C The subroutine will take as parameterthe information 
C necessary to build up the projection 
	  subroutine area(name, projection, lllat, lllon, 
     +                urlat, urlon, vertlon, hemis)

	  character*18 name
	  character*19 projection
	  real lllon, lllat
	  real urlon, urlat
	  real vertlon
	  character*6 hemis

C Create a new page in the postscript
	  call pnew('page')
C define the coordinates of the geographical area 
      call psetc('subpage_map_projection', projection)
      call psetr('subpage_lower_left_longitude', lllon)
      call psetr('subpage_upper_right_longitude', urlon)
      call psetr('subpage_upper_right_latitude', urlat)
      call psetr('subpage_lower_left_latitude', lllat)
      call psetr('subpage_map_vertical_longitude', vertlon)

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
      call psetc('text_line_1', name)
	  call pseti('text_line_count', 1)

C Alternative setting : using an array of string instead
c     call pset1c('text_lines', (/'Global Map!'/), 1)

      call ptext
      
	  end
