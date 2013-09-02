      PROGRAM LANDSEA_SHADING

C     No data is plotted, just the global coastline
C     We switch land and sea shading on and off to see what happens..


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('landsea_shading')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     



C     First page: land and sea shading together:

      CALL PSETC ('TEXT_LINE_1', 'LAND and SEA shading')
      CALL PSETR ('TEXT_FONT_SIZE',                   2.0)
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'ON')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE',         'ON')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE_COLOUR',  'BLUE')
      CALL PCOAST
      CALL PTEXT


C     Second page: land shading only:

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('TEXT_LINE_1', 'LAND shading only')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'ON')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE',         'OFF')
      CALL PCOAST
      CALL PTEXT


C     Third page: sea shading only:

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('TEXT_LINE_1', 'SEA shading only')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE',         'ON')
      CALL PCOAST
      CALL PTEXT


C     Fourth page:  land and sea shading together, zoom in:

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('TEXT_LINE_1', 'LAND and SEA shading')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',      30.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    -30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',     65.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',    70.0)
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'ON')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE',         'ON')
      CALL PCOAST
      CALL PTEXT


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
