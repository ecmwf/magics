      PROGRAM WIND_LANDSHADE

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('wind_landshade')


C     Set up all the parameters we'll use in all the examples

      CALL PSETC ('MAP_COASTLINE_COLOUR',            'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',                 'TAN')
      CALL PSETC ('MAP_LABEL_COLOUR',                'TAN')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'ON')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE',         'ON')
      CALL PSETC ('MAP_COASTLINE_SEA_SHADE_COLOUR',  'SKY')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -25.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -40.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   38.0)


C     Load the wind data

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/uv500.grb')
      CALL PGRIB




C     Plot LAND and SEA shading with wind ARROWS

      CALL PSETC ('TEXT_LINE_1',
     +            'Wind arrows with LAND and SEA shading')

      CALL PCOAST 
      CALL PWIND
      CALL PTEXT




C     Plot SEA shading with wind ARROWS

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE', 'OFF')
      CALL PSETC ('TEXT_LINE_1',
     +            'Wind arrows with SEA shading')

      CALL PCOAST 
      CALL PWIND
      CALL PTEXT



C     Plot LAND and SEA shading with wind FLAGS

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE', 'ON')
c      CALL PSETC ('WIND_FIELD_TYPE', 'FLAGS')

      CALL PSETC ('TEXT_LINE_1',
     +            'Wind flags with LAND and SEA shading')

      CALL PCOAST 
      CALL PWIND
      CALL PTEXT



      CALL PCLOSE
      END


#include "parse_command_line.h"
