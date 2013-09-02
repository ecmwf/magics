      PROGRAM DATAT3999RGG2000

C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_t3999_rgg2000')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/orog.3999.grib')
      CALL PGRIB
      

C     Define the contour     

      CALL PCONT


C     Set up and plot the title text

      CALL PTEXT


C     Plot the coastlines and then close

      CALL PCOAST
      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
