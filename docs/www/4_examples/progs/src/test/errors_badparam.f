      PROGRAM ERRORS_BADPARAM


      CALL POPEN
      CALL PARSE_COMMAND_LINE ('errors_badparam')



C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

c      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
c      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/total_precip.grib')
c      CALL PGRIB
      

C     Set a non-existant parameter

      CALL PSETC  ('BAD_PARAMETER',      'BLUE')



C     Plot everything

c      CALL PCONT
      CALL PCOAST
c      CALL PTEXT


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

