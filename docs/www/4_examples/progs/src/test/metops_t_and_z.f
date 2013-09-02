      PROGRAM METOPS_T_AND_Z


      PARAMETER (NLEV=20)
      CHARACTER*25 CTAB
      DIMENSION  CTAB  (NLEV)
C      DATA       CTAB  /'RGB(0.57, 0.21, 0.04)',
C     +                  'RGB(0.57, 0.21, 0.71)',
C     +                  'RGB(0.65, 0.65, 0.97)',
C     +                  'RGB(0.95, 0.95, 0.0)',
C     +                  'RGB(0.8,  0.95, 0.37)',
C     +                  'RGB(0.61, 0.78, 0.37)',
C     +                  'RGB(0.46, 0.61, 0.37)'/
      DATA       CTAB  /'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'GREENISH_BLUE',
     +                  'BLUE_GREEN',
     +                  'BLUISH_GREEN',
     +                  'YELLOW_GREEN',
     +                  'GREENISH_YELLOW',
     +                  'YELLOW',
     +                  'ORANGISH_YELLOW',
     +                  'ORANGE_YELLOW',
     +                  'YELLOWISH_ORANGE',
     +                  'ORANGE',
     +                  'REDDISH_ORANGE',
     +                  'RED_ORANGE',
     +                  'ORANGISH_RED',
     +                  'RED',
     +                  'MAGENTA',
     +                  'MAGENTA'/


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('metops_t_and_z')


C     Area specification (SOUTH, WEST, NORTH, EAST )

C      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    23.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -33.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   51.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  72.0)


C     First, load and plot the temperature data, shaded

C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/t850_fc_12.grib')
      CALL PGRIB
      

C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')     


C     Define the contour     

      CALL PSETC  ('CONTOUR',                      'OFF')
      CALL PSETR  ('CONTOUR_SHADE_MIN_LEVEL',       -40.)
      CALL PSETR  ('CONTOUR_SHADE_MAX_LEVEL',        44.)
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',              4.0)
c     CALL PSETI  ('CONTOUR_HIGHLIGHT_THICKNESS',   5)
c     CALL PSETI  ('CONTOUR_HIGHLIGHT_FREQUENCY',   100)
c     CALL PSETC  ('CONTOUR_HIGHLIGHT_STYLE',      'DASH')
c     CALL PSETC  ('CONTOUR_LINE_STYLE',           'DOT')
c     CALL PSETI  ('CONTOUR_LINE_THICKNESS',        3)
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_METHOD',  'LIST')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_LIST',     CTAB, NLEV)
      CALL PSETC  ('CONTOUR_SHADE',                'ON')      
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'POLYGON_SHADING')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',         'AREA_FILL')
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PCONT
      

C     Plot the title text and the coastlines

      CALL PSETR  ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.4)
      CALL PTEXT
      CALL PCOAST



C     Now load the Z500 data

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500_fc_12.grib')
      CALL PGRIB


C     Redefine the contouring parameters for the Z500

      CALL PSETC  ('CONTOUR',                      'ON')
      CALL PSETR  ('CONTOUR_INTERVAL',              6.0)
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETC  ('CONTOUR_LINE_COLOUR',          'BLACK')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR',     'BLACK')
      CALL PSETI  ('CONTOUR_HIGHLIGHT_THICKNESS',   4)
c     CALL PSETC  ('CONTOUR_LABEL_COLOUR',         'BLACK')
      CALL PSETC  ('CONTOUR_HILO',                 'ON')
      CALL PSETR  ('CONTOUR_HILO_HEIGHT',           0.25)
      CALL PSETR  ('CONTOUR_HILO_SUPPRESS_RADIUS',  40.0)
c     CALL PSETC  ('CONTOUR_HI_COLOUR',            'BLACK')
c     CALL PSETC  ('CONTOUR_LO_COLOUR',            'BLACK')
      CALL PSETC  ('CONTOUR_SHADE',                'OFF')      
      CALL PCONT


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
