      PROGRAM WIND

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('wind_ex')


C     Set up all the parameters we'll use in all the examples

      CALL PSETC ('MAP_COASTLINE_COLOUR',            'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',                 'TAN')
      CALL PSETC ('MAP_LABEL_COLOUR',                'TAN')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    20.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -20.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   80.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  80.0)


C     Load the data

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/uv100.grb')


      CALL PSETI ('TEXT_LINE_COUNT',     1)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      CALL PSETC ('LEGEND',             'ON')



C     Start a new page, this time showing the wind arrows

      CALL PNEW  ('SUPER_PAGE')

      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)


C     Plot the wind field using default wind-plotting paramters

      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, no thinning')

      CALL PCOAST 
      CALL PGRIB
      CALL PWIND
      CALL PTEXT




C     Start a new page, this time with no thinning

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('WIND_THINNING_FACTOR', 1.0)
      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, thinning 1.0')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT


C     Start a new page, this time with thinning

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('WIND_THINNING_FACTOR', 2.6)
      CALL PSETC ('TEXT_LINE_1', 'RED wind arrows, thinning 2.6')
      CALL PSETC ('WIND_ARROW_COLOUR',   'RED')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT


C     Start a new page, this time with thinning

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('WIND_THINNING_FACTOR',  8.0)
      CALL PSETR ('WIND_ARROW_CALM_BELOW', 2.0)
      CALL PSETC ('WIND_ARROW_CALM_INDICATOR', 'ON')
      CALL PSETC ('WIND_ARROW_COLOUR', 'BLUE')
      CALL PSETI ('TEXT_LINE_COUNT' ,2)
      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, thinning 8.0')
      CALL PSETC ('TEXT_LINE_2', 'Calm below 2.0')
      CALL PCOAST 
      CALL PWIND
      CALL PTEXT


      CALL PCLOSE
      END


#include "parse_command_line.h"
