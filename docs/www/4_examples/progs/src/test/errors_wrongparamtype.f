      PROGRAM ERRORS_WRONGPARAMTYPE

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('errors_wrongparamtype')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETR ('MAP_GRID_COLOUR',      'GREY')     



C     Plot everything

      CALL PCOAST


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

