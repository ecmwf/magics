      PROGRAM DATAGAUSSIAN


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_gaussian')


C     Set the geographical area to plot

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    40.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -6.7)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   68.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  25.0)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS - a regular Gaussian grid

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     +            'data/regular_gaussian_q300.grib')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC  ('CONTOUR_HILO',             'OFF')
      CALL PCONT


C     Set up and plot the title text

      CALL PTEXT


C     Plot the coastlines and then close

      CALL PCOAST



C     -------- New page, new data --------------

      CALL PNEW ('SUPER_PAGE')


C     Pass the data to MAGICS - a reduced Gaussian grid

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/gaussian_q300.grib')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC  ('CONTOUR_HILO',             'OFF')
      CALL PCONT


C     Set up and plot the title text

      CALL PTEXT


C     Plot the coastlines and then close

      CALL PCOAST


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
