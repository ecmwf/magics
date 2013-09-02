      PROGRAM POLARSTEREO

C     This program demonstrates magics contouring facilities. 
C     The meteorological data field is a standard global 500 hpa
C     model output field on a regular 1.5 degree grid. 
C     Contours and coastlines are projected onto a 
C     polar stereographic map.


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('proj_polarstereo_ex')


C     Set up the polar stereographic projection

c      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
C      CALL PSETC ('GRIB_AUTOMATIC_SCALING', 'OFF')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PCONT
      

C     Set up and plot the title text

      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 1.0)
      CALL PSETC ('TEXT_LINE_1',
     +            'Polar Stereographic, Northern Hemisphere')
      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT


C     Plot the coastlines

      CALL PCOAST


C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('SUBPAGE_MAP_AREA_DEFINITION', 'CENTRE')
      CALL PSETR ('SUBPAGE_MAP_CENTRE_LONGITUDE',  0.0)
      CALL PSETR ('SUBPAGE_MAP_CENTRE_LATITUDE',  50.0)
      CALL PSETC ('SUBPAGE_MAP_HEMISPHERE',      'NORTH')

      CALL PSETC ('TEXT_LINE_1',
     +            'Polar Stereographic Projection, section')

C      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',   20.)
C      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  20.)
C      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', 80.)
C      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',  80.)

      CALL PCONT
      CALL PTEXT
      CALL PCOAST


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

