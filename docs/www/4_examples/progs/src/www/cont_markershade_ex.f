      PROGRAM CONTMARKERSHADE

C     This program demonstrates magics contouring facilities with solid shading. 
C     The meteorological data field is a standard global 500 hpa
C     model output field on a regular 1.5 degree grid. 
C     Contours and coastlines are projected onto a 
C     polar stereographic map.


C     Set up our markers

      CHARACTER*10 CTAB
      DIMENSION CTAB(6), HTAB(6), MTAB(6)
      DATA CTAB /'RED', 'ORANGE', 'OLIVE', 'GREEN',
     x           'BLUE_GREEN', 'BLUE'/
      DATA MTAB /1, 2, 3, 4, 5, 6/
      DATA HTAB /0.22, 0.23, 0.21, 0.18, 0.17, 0.15/

 
C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_markershade_ex')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    55.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -10.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   10.0)


C     Set up the coastline attributes 

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_SCALING', 'OFF')
      CALL PSETC ('GRIB_SPECIFICATION', 'OFF')
      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/z500.grb')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('CONTOUR_LINE_COLOUR',       'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR',  'BLUE')
      CALL PSETC  ('CONTOUR_SHADE',             'ON')      
      call PSETC  ('CONTOUR_SHADE_TECHNIQUE',   'MARKER')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_TABLE', CTAB, 6)
      CALL PSET1R ('CONTOUR_SHADE_HEIGHT_TABLE', HTAB, 6)
      CALL PSET1I ('CONTOUR_SHADE_MARKER_TABLE', MTAB, 6)
      CALL PCONT
      

C     Set up and plot the title text

      CALL PSETC ('TEXT_LINE_1',       'Contours with marker shading')
      CALL PSETI ('TEXT_LINE_COUNT',   1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT


C     Plot the coastlines and then close

      CALL PCOAST
      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

