      program  magics



C Open Magics
      call ??? 

C define the name of the output 
      call psetc('output_name', 'hello_world_fortran')

C find and call the action routine to plot the coastlines
      call ???

C define the text 
      call psetc('???', 'Hello World!')
      call pseti('text_lines_count', 1)
C Alternative setting : using anarray of string instead 
	  call pset1c('???', (/'Hello World!'/), 1)

C find and call the action routine to display the text
      call ???

C Close magics
      call ???

      end
