      PROGRAM odbt2mex

      real      clist(11), hlist(8)
      character ctable*20
      integer   mtable(1)
      dimension ctable(8)
      real      min(8), max(8)
      
      
      data min    /220., 233., 245., 258., 270., 283., 
     +             295., 308./
               
      data max    /233., 245., 258., 270., 283., 295.,
     +             308., 320./

      data hlist  /0.2, 0.2, 0.3, 0.1, 0.2, 0.3, 0.1, 0.2/

      data ctable /'NAVY',   'BLUE',   'CYAN', 'GREEN',
     +             'YELLOW', 'ORANGE', 'RED',  'PURPLE'/
     
      data mtable /4/


C     Start MAGICS and set the ouput device

      call popen
      CALL PARSE_COMMAND_LINE ('odb_t2m_ex')
      
      call psetc ('automatic_title', 'on')
      call psetr ('text_reference_character_height', 0.4)



C     Set up the coastline attributes

      call psetc ('map_coastline',        'on')
      call psetc ('map_coastline_colour', 'grey')
      call psetc ('map_grid_colour',      'grey')     


C     Access the ODB and pass the data to MAGICS

      call psetc('odb_database',
     +             'odb://njord/tmp/odb_data/ECMA.conv/ECMA')

      call psetc ('odb_query',
     +              'select lat, lon, obsvalue 
     +               from hdr, body 
     +               where varno=$t2m and obsvalue is not null')
      
      call psetc ('odb_latitude',    'lat@hdr')
      call psetc ('odb_longitude',   'lon@hdr')
      call psetc ('odb_observation', 'obsvalue@body')
      call pseti ('odb_nb_rows',      200000)
      call podb
      

C     Define the symbols  
      
      call psetc  ('symbol_table_mode',  'on')
      call pset1r ('symbol_min_table',    min, 8)
      call pset1r ('symbol_max_table',    max, 8)
      call pset1r ('symbol_height_table', hlist, 8)    
      call pset1i ('symbol_marker_table', mtable, 1)     
      call pset1c ('symbol_colour_table', ctable, 8)  

      call psymb
      

C     Set up and plot the title text
     
      call ptext
      call pcoast



C     ------------------------------------------------------------
C     New page - this time with a smaller request and an area zoom
C     ------------------------------------------------------------

      call pnew  ('super_page')


      call psetr ('subpage_lower_left_longitude',  -20.0)
      call psetr ('subpage_upper_right_longitude',  20.0)
      call psetr ('subpage_upper_right_latitude',   50.0)
      call psetr ('subpage_lower_left_latitude',    30.0)

      call psetc('odb_database',
     +             'odb://njord/tmp/odb_data/ECMA.conv/ECMA')

      call psetc ('odb_query',
     +              'select lat, lon, obsvalue 
     +               from hdr, body 
     +               where varno=$t2m and 
     +               degrees(lat) < 48 and degrees(lat) >  32 and
     +               degrees(lon) < 18 and degrees(lon) > -18 and
     +               obsvalue is not null')
      
      call psetc ('odb_latitude',    'lat@hdr')
      call psetc ('odb_longitude',   'lon@hdr')
      call psetc ('odb_observation', 'obsvalue@body')
      call pseti ('odb_nb_rows',      200000)
      call podb


      call psymb
      call pcoast

      call pclose

      STOP
      END



#include "parse_command_line.h"

