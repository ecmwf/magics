      PROGRAM COASTONLY_1

C     No data is plotted, just the coastline grid


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('coastonly_1')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Plot the coastlines and then close

      CALL PCOAST
      CALL PCLOSE

      STOP
      END


#include "parse_command_line.h"
