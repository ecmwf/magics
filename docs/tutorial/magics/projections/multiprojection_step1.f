
C The subroutine will take as parameters the information 
C necessary to build up the projection 
C name: title
C projection: projection name
C lllat: lower left latitude
C lllon: lower left longitude
C urlat: upper right latitude
C urlat: upper right longitude
C vertlon: vertical longitude ( for polar stereo) 
C hemis: hemisphere  ( for polar stereo) 

	  subroutine area(name, projection, lllat, lllon, 
     +                urlat, urlon, vertlon, hemis)

	  character*18 name
	  character*19 projection
	  real lllon, lllat
	  real urlon, urlat
	  real vertlon
	  character*6 hemis

C Create a new page in the postscript
	  call ???('???')
C define the coordinates of the geographical area 
      call psetc('subpage_map_projection', ???)
      call psetr('subpage_lower_left_longitude', ???)
      call psetr('subpage_upper_right_longitude', ???)
      call psetr('subpage_upper_right_latitude', ???)
      call psetr('subpage_lower_left_latitude', ???)
      call psetr('subpage_map_vertical_longitude',???)

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


      call ???
      end
