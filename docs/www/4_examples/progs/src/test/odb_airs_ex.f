      PROGRAM odbairs

      real clist(11), hlist(1)
      character ctable*20
      integer mtable(1)
      dimension ctable(10)
      real min(8), max(8)
      
      
      data min    /-20., -15., -10., -5., 0., 5.,  10., 15./
               
      data max    / -15., -10., -5., 0., 5.,  10., 15., 20./

      data hlist  /0.05/

      data ctable /'navy',   'blue',   'greenish_blue', 'cyan',
     +             'green',  'yellow', 'orange',        'red_orange',
     +             'red',    'purple'/

      data mtable /2/


C     Start MAGICS and set the ouput device

      call popen
      CALL PARSE_COMMAND_LINE ('odb_airs_ex')
      
      call psetc ('automatic_title', 'on')
      call psetr ('text_reference_character_height', 0.4)



C     Set up the coastline attributes

      call psetc ('map_coastline',        'on')
      call psetc ('map_coastline_colour', 'grey')
      call psetc ('map_grid_colour',      'grey')     


C     Access the ODB and pass the data to MAGICS

      call psetc('odb_database',
     +             'odb://njord/tmp/odb_data/ECMA.airs/ECMA')

      call psetc ('odb_query',
     +              'select distinct lat, lon, obsvalue, fg_depar
     +               from hdr, body 
     +               where obstype=7 and press=221')
      
      call psetc ('odb_latitude',    'lat@hdr')
      call psetc ('odb_longitude',   'lon@hdr')
      call psetc ('odb_observation', 'fg_depar@body')
      call pseti ('odb_nb_rows',      200000)
      call podb
      

C     Define the contour  
      
      call psetc  ('symbol_table_mode',  'on')
      call pset1r ('symbol_min_table',    min, 8)
      call pset1r ('symbol_max_table',    max, 8)
      call pset1r ('symbol_height_table', hlist, 1)    
      call pset1i ('symbol_marker_table', mtable, 1)     
      call pset1c ('symbol_colour_table', ctable, 8)  

      call psymb
      

C     Set up and plot the title text
     
      call ptext
      call pcoast


      call pclose

      STOP
      END


#include "parse_command_line.h"
