      PROGRAM CONTSOLIDSHADE

C     This program demonstrates magics contouring facilities with solid shading. 
C     The meteorological data field is a standard global 500 hpa
C     model output field on a regular 1.5 degree grid. 
C     Contours and coastlines are projected onto a 
C     polar stereographic map.


      PARAMETER (NLEV=12)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /480., 490., 500., 510., 520., 530.,
     +                 540., 550., 560., 570., 580., 593./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_solidshade_ex')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('LEGEND',                       'ON')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      
      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC  ('CONTOUR_SHADE',            'ON')      
      call PSETC  ('CONTOUR_SHADE_TECHNIQUE',  'POLYGON_SHADING')
      call PSETC  ('CONTOUR_SHADE_METHOD',     'AREA_FILL')
      CALL PCONT
      

C     Set up and plot the title text

      CALL PSETC ('TEXT_LINE_1', 'Contours with solid shading')
      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT


C     Plot the coastlines

      CALL PCOAST


C     ------------------------------------------------------------
C     New page - this time with a smaller area
C     ------------------------------------------------------------

      CALL PNEW ('SUPER_PAGE')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    30.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   65.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  70.0)


C     Plot everything

      CALL PTEXT
      CALL PCOAST
      CALL PCONT




      CALL PCLOSE

      STOP
      END


#include "parse_command_line.h"
