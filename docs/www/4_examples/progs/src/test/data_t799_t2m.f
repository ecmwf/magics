      PROGRAM DATAT799T2M

C     Tests T799 data in full resolution (0.225 degree grid)

C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_t799_t2m')


C     Set up the coastline attributes and plot

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK') 


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/t799_t2m_ll.grib')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
      CALL PGRIB
      


C     Draw the contour     

      CALL PSETC  ('CONTOUR',                        'OFF')
      CALL PSETC  ('CONTOUR_SHADE',                  'ON')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE',   'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',                1.0)
      CALL PSETC  ('CONTOUR_SHADE_MIN_LEVEL_COLOUR', 'BLUE')
      CALL PSETC  ('CONTOUR_SHADE_MAX_LEVEL_COLOUR', 'ROSE')
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_DIRECTION', 'CLOCKWISE')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',           'AREA_FILL')
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',        'CELL_SHADING')
c      CALL PSETR ('CONTOUR_SHADE_CELL_RESOLUTION', 1.0)
      CALL PSETC  ('CONTOUR_HILO',                   'OFF')
      CALL PCONT


C     Set up and plot the title text

      CALL PTEXT
      CALL PCOAST



C     New page, this time zoomed in

      CALL PNEW ('PAGE')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     25.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -15.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  -10.0)

      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE', 'BOTH')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',  'ON')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 4)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 4)
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
