      program  magics

C open Magics
      call popen

C Define the name of the output 
      call ???('output_name', 'multi_projection')

C Call the subroutine area defined later in the program 
C The routine is expecting the following parameters
C      Title to display
C      Projection name
C      lower left latitude
C      lower left longitude
C      upper right latitude
C      upper right longitude
C      Vertical longitude ( useful for polar stereographic )
C      Hemisphere ( useful for polar stereographic )

      call area('Global Cylindrical', 'cylindrical        ', 
     + ???, ???., ???, ???, 'north')
	  call area('Europe            ', 'polar_stereographic', 
     + ???, ???., ???, ???, 'north')
      call area('North America     ', 'polar_stereographic', 
     + ???, ???., ???, ???, 'north')
      call area('South America     ', 'cylindrical        ', 
     + ???, ???., ???, ???, 'north')
      call area('Asia              ', 'cylindrical        ', 
     + ???, ???., ???, ???, 'north')
      call area('Australia         ', 'cylindrical        ', 
     + ???, ???., ???, ???, 'north')
      call area('Africa            ', 'cylindrical        ', 
     + ???, ???., ???, ???, 'north')

      call ???

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

C Create a new page in the postscript§
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
