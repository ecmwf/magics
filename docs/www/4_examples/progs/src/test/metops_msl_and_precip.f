      PROGRAM METOPS_MSL_AND_PRECIP


      PARAMETER (NLEV=8)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /1., 4., 8., 20., 50., 100., 200., 600./
      CHARACTER*25 CTAB
      DIMENSION  CTAB  (NLEV)
      DATA       CTAB  /'HSL(180,1,0.75)',
     +                  'HSL(210,1,0.75)',
     +                  'HSL(240,1,0.75)',
     +                  'HSL(270,1,0.75)',
     +                  'HSL(300,1,0.75)',
     +                  'HSL(330,1,0.75)',
     +                  'HSL(0,1,0.75)',
     +                  'HSL(0,1,0.75)'/


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('metops_msl_and_precip')


C     Area specification (SOUTH, WEST, NORTH, EAST )

C      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    23.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -33.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   51.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  72.0)



C     First, load and plot the temperature data, shaded

C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/total_precip.grib')
      CALL PGRIB
      
      
C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')     


C     Define the contour     

      CALL PSETC  ('LEGEND',                       'ON')
      CALL PSETC  ('CONTOUR',                      'OFF')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'OFF')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_METHOD',  'LIST')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_LIST',     CTAB, NLEV)
      CALL PSETC  ('CONTOUR_SHADE',                'ON')      
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'POLYGON_SHADING')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',         'AREA_FILL')
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PSETC  ('CONTOUR_LABEL',                'OFF')
      CALL PCONT
      

C     Plot the title text and the coastlines

      CALL PSETR  ('TEXT_REFERENCE_CHARACTER_HEIGHT',  0.35)
      CALL PTEXT
      CALL PCOAST



C     Now load the MSL data

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/msl.grib')
      CALL PGRIB


C     Redefine the contouring parameters for the MSL

      CALL PSETC  ('CONTOUR',                      'ON')
      CALL PSETR  ('CONTOUR_INTERVAL',              5.0)
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETC  ('CONTOUR_LINE_COLOUR',          'BLACK')
      CALL PSETI  ('CONTOUR_LINE_THICKNESS',        2)
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR',     'BLACK')
      CALL PSETI  ('CONTOUR_HIGHLIGHT_THICKNESS',   4)
      CALL PSETC  ('CONTOUR_LABEL_COLOUR',         'BLACK')
      CALL PSETC  ('CONTOUR_SHADE',                'OFF')      
      CALL PSETC  ('CONTOUR_HILO',                 'ON')
      CALL PSETR  ('CONTOUR_HILO_SUPPRESS_RADIUS',  40.0)
      CALL PSETR  ('CONTOUR_HILO_HEIGHT',           0.25)
      CALL PSETC  ('CONTOUR_HI_COLOUR',            'BLACK')
      CALL PSETC  ('CONTOUR_LO_COLOUR',            'BLACK')
      CALL PSETC  ('CONTOUR_LABEL',                'ON')
      CALL PCONT


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
