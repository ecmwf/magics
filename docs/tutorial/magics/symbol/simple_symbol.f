      program  magics

C Define the title
      character*25 var
      dimension var(1)
      data var / +    'My first symbol plotting' /

C Open Magics
      call popen

C set the outut information
      call psetc(???)

C Define the background coastlines
      call pset?(???)
      call pcoast

C Load the data 
      call psetc('geo_input_file_name', '../airep.geo')
      call pgeo

c Define the attributes of the symbol plotting
      call psetc('symbol_type', ???)
      call pseti('symbol_marker', ???)
      call psetc('symbol_colour', ???)
      call psetr('symbol_height', ???)
      call psymb

C Add a text
      call pset?(???)
      call ptext


C Close magics : do the plot
      call ???

      end
