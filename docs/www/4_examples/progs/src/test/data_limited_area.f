      PROGRAM DATAT LIMITEDAREA

C     Tests T1279 data in full resolution (0.??? degree grid)

C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_limited_area')


C     Set up the coastline attributes and plot

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY') 


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/R1D02050000020600001')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
c      CALL PGRIB
c should not be necessary, but MAGICS 6 likes it!

      DO 200 IPAGE = 1,6

C       Load the next field and plot the image

        CALL PGRIB
        CALL PCOAST
        CALL PCONT
        CALL PTEXT

        CALL PNEW('PAGE')

200   CONTINUE



C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
