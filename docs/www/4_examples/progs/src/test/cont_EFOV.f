      PROGRAM CONTEFOV

C     Although this data file is not used in practice, it
C     serves as an interesting example of VAREPS data because
C     in Metview, MAGICS produces some slightly wrong contours.
C     As it turns out, when used standalone, MAGICS does some
C     very strange things....


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_EFOV')


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/EFOV_1_5_old.grib')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PCONT


C     Set up and plot the title text

      CALL PTEXT


C     Plot the coastlines

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     
      CALL PCOAST


C     A new page with a new geographical area

      CALL PNEW  ('SUPER_PAGE')

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    25.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   65.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', 145.0)

      CALL PCONT
      CALL PCOAST
      CALL PTEXT


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
