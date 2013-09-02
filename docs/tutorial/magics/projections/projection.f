      program  magics

C open Magics
      call popen

C Define the name of the output 
      call ???('???', 'projection_fortran')

C define the coordinates of the geographical area 
      call psetc('subpage_map_projection', ???)
      call psetr('subpage_lower_left_longitude', ???)
      call ???('subpage_upper_right_longitude', ???)
      call psetr('???', ???)
      call psetr('???', ???)

C define the coastlines attributes 
      call psetc('colour of the line', '???')
      call psetc('land shading on???', '???')
      call psetc('sea shading on???', 'on')
      call ???('which colour for the land shading?', '???')
      call psetc('which colour for the See shading?', '???')
      call psetc('We want the grid', '??')
      call psetc('We want a grey grid', '???')
      call psetc('We do not want label', 'off')
	
C Call the action routine to plot the coastlines
      call ???

C Setting of the title 
      call pset1c('???', (/'Global Map!'/), 1)

      call ???

      call pclose

      end
