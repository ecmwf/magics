      PROGRAM WIND_VO_D

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('wind_vo_d')


C     Set up the coastline

      CALL PSETC ('MAP_COASTLINE_COLOUR',            'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',                 'TAN')
      CALL PSETC ('MAP_LABEL_COLOUR',                'TAN')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')


C     Set up the data input

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/vd850.grib')
      CALL psetc ('GRIB_WIND_MODE',       'VD')

      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)


C     Set up the title text

      CALL PSETI ('TEXT_LINE_COUNT',     2)
      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, no thinning')
      CALL PSETC ('TEXT_LINE_2', 'Vorticity / Divergence wind plot')
      CALL PSETC ('LEGEND', 'ON')


C     Call the action routines

      CALL PCOAST 
      CALL PGRIB
      CALL PWIND
      CALL PTEXT



      CALL PCLOSE
      END


#include "parse_command_line.h"
