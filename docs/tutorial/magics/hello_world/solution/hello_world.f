      program  magics

C open Magics
      call popen

C Define the name of the output 
      call psetc('output_name', 'hello_world')

C Call the action routine to plot the coastlines
      call pcoast

C define the text to plot 
      call psetc('text_line_1', 'Hello World!')
	  call pseti('text_line_count', 1)

C Alternative setting : using an array of string instead
c     call pset1c('text_lines', (/'Hello World!'/), 1)

      call ptext

      call pclose

      end
