      PROGRAM CONTLINEARRES

C     This program is intended to illustrate how the LINEAR contouring
C     method copes with different scenarios. The question we are asking
C     is: under what circumstances is it adequate?


      PARAMETER (NLEV=9)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /235.,237., 239., 241., 243., 245., 247.,
     x                 249.,251./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_linear_res')


C     Set up the paper size

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 21.0)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 29.7)



C     Set up the coastline attributes

      CALL PSETC ('MAP_GRID_COLOUR',              'GREY') 
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   5.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  5.0)
      CALL PSETC ('MAP_COASTLINE',                'OFF')


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',     'FILE')
      CALL PSETC ('GRIB_SPECIFICATION',  'OFF')
      CALl PSETC ('GRIB_SCALING',        'OFF')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/t300.grb')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC ('CONTOUR_METHOD',           'LINEAR')
      CALL PSETR ('AKIMA_RESOLUTION_X',        0.2)
      CALL PSETR ('AKIMA_RESOLUTION_Y',        0.2)
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC ('CONTOUR_LABEL_COLOUR',     'BLACK')
      CALL PSETI ('CONTOUR_LABEL_FREQUENCY',   1)
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R('CONTOUR_LEVEL_LIST',            RLEV, NLEV)
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',  'OFF')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 4)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 4) 
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',        0.4)
      CALL PSETC ('CONTOUR_HILO',                    'OFF')
      CALL PCONT
      

C     Set up the title text

      CALL PSETI ('TEXT_LINE_COUNT',     1)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')




C     First page, this time using the global field

      CALL PSETC ('TEXT_LINE_1',
     x            'LINEAR Contouring using global field')

      CALL PSETC ('CONTOUR_METHOD',           'LINEAR')
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PCONT

c      CALL PSETC ('CONTOUR_METHOD',           'AKIMA760')
c      CALL PSETC ('CONTOUR_LINE_COLOUR',      'RED')
c      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'RED')
c      CALL PCONT

      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -68.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -135.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    68.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  135.0)

      CALL PSETC ('TEXT_LINE_1', 
     x            'LINEAR Contouring, SWNE = -68.0, -135, 68, 135')

      CALL PSETC ('CONTOUR_METHOD',           'LINEAR')
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PCONT

      CALL PSETC ('CONTOUR_METHOD',           'AKIMA760')
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'RED')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'RED')
      CALL PCONT

      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',   -45.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -90.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   45.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  90.0)

      CALL PSETC ('TEXT_LINE_1', 
     x            'LINEAR Contouring, SWNE = -45.0, -90, 45, 90')

      CALL PSETC ('CONTOUR_METHOD',           'LINEAR')
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PCONT

      CALL PSETC ('CONTOUR_METHOD',           'AKIMA760')
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'RED')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'RED')
      CALL PCONT

      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -20.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -45.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    20.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   45.0)

      CALL PSETC ('TEXT_LINE_1', 
     x            'LINEAR Contouring, SWNE = -20, -45, 20, 45')

      CALL PSETC ('CONTOUR_METHOD',           'LINEAR')
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PCONT

      CALL PSETC ('CONTOUR_METHOD',           'AKIMA760')
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'RED')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'RED')
      CALL PCONT

      CALL PTEXT
      CALL PCOAST



C     Close

      CALL PCLOSE

      STOP
      END


#include "parse_command_line.h"
