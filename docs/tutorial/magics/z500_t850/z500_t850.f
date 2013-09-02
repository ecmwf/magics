      program  magics

C Defines the colours table for the shading
      character*25 colour
      dimension colour(24)

C Defines the lines for the title
      character*100 title
      dimension title(4)

      data colour /
     +    'rgb(0,0,0.5)', 
     +    'rgb(0,0,0.5)', 
     +    'rgb(0,0,0.85)', 
     +    'rgb(0,0,0.5)', 
     +    'rgb(0,0,0.85)', 
     +    'rgb(0.25,0,1)', 
     +    'blue_purple', 
     +    'greenish_blue', 
     +    'blue_green', 
     +    'bluish_green', 
     +    'yellow_green', 
     +    'greenish_yellow', 
     +    'yellow', 
     +    'orangish_yellow', 
     +    'orange_yellow', 
     +    'yellowish_orange', 
     +    'orange', 
     +    'reddish_orange', 
     +    'red_orange', 
     +    'orangish_red', 
     +    'red', 
     +    'magenta', 
     +    'magenta', 
     +    'magenta' /

      data title /
     +    "<grib_info id='z500' key='base-date' ",
     +    "<grib_info id='z500' key='valid-date' ",
     +    "parameter:<grib_info id='z500' key='name'/>",
     +    "parameter:<grib_info id='t850' key='name'/>"/

C open magics
      call popen

C Setting of the output file name
      call ???('???', '???')

C Setting the coordinates of the geographical area
C  For example take Europe 
      call psetr
      call psetr
	  .....
      call psetc('subpage_map_projection', '???)

C Import the t850 data
      call psetc('grib_id', 't850')
      call psetc('grib_input_file_name', '../t850.grb')
      call ???

C Define the shading for t850 
C find the parameters to set
	  call psetc...
	  call psetr...
	  call pseti...
      call pcont


C Import the z500 data
      call psetc('???', 'z500')
      call psetc('????', '../z500.grb')
      call pgrib

C Define the contouring for z500 
C find the parameters to set
	  call psetc...
	  call psetr...
	  call pseti...
      call pcont


C Add the coastlines 
      call psetc('map_grid_colour', 'tan')
      call psetc('map_grid', 'on')
      call psetc('map_coastline_colour', 'tan')
      call pcoast


C add a title
      call pset1c('text_lines', title, 4)
      call psetc('text_justification', 'left')
      call psetc('text_html', 'true')
      call psetc('text_colour', 'black')
      call ptext

C In a second step: try to position text whereever you want 
C     call psetc('text_mode', 'positional')
C     call psetr('text_box_y_length', ???)
C     call psetc('text_border', '???')
C     call psetr('text_box_x_length', ???)
C     call psetr('text_box_x_position', ???)
C     call psetc('text_box_blanking', 'on')
C     call psetc('text_border_colour', 'black')
C     call psetr('text_box_y_position', ???)

C Add a legend
      call psetc('legend', 'on')
      call psetc('legend_display_type', 'continuous')

C In a second step:  the Try to move it around!
C     call psetr('legend_box_x_position', ???)
C     call psetr('legend_box_y_position', ??)
C     call psetr('legend_box_x_length', ???)
C     call psetc('legend_box_mode', 'positional')
C     call psetr('legend_box_y_length', ???)


      call pclose

      end
