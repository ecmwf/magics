      PROGRAM DATAT1279GG640

C     Tests T1279 data in full resolution (0.??? degree grid)

C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_t1279_gg640')


C     Set up the coastline attributes and plot

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY') 
      CALL PCOAST


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     +            'data/t1279_gg640_lev85.grib')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
      CALL PGRIB
      

C     Draw the contour     

      CALL PSETR  ('CONTOUR_HILO_REDUCTION_RADIUS', 90.0)
      CALL PSETR  ('CONTOUR_HILO_SUPPRESS_RADIUS',  90.0)
      CALL PCONT


C     Set up and plot the title text

      CALL PTEXT



C     New page, this time zoomed in

      CALL PNEW ('PAGE')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     68.5)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',     7.5)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   10.0)

      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE', 'BOTH')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',  'ON')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 1)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 1)
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',        0.3)
      CALL PSETR ('CONTOUR_GRID_VALUE_MARKER_HEIGHT', 0.2)
      CALL PCONT

      CALL PTEXT

      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',  0.5)    
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT', 0.5)    
      CALL PCOAST


C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
