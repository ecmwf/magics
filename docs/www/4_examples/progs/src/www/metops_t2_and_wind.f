      PROGRAM METOPS_T2_AND_WIND

      PARAMETER (NLEV=25)
      CHARACTER*25 CTAB
      DIMENSION  CTAB  (NLEV)
      DATA       CTAB  /'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
     +                  'BLUE_PURPLE',
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
      CALL PARSE_COMMAND_LINE ('metops_t2_and_wind')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    23.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -33.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   51.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  72.0)




C     First, load and plot the temperature data, shaded

C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/t2m_fc12.grib')
      CALL PGRIB
      

C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')     


C     Define the contour     

      CALL PSETC  ('CONTOUR',                      'OFF')
      CALL PSETR  ('CONTOUR_SHADE_MIN_LEVEL',       -60.)
      CALL PSETR  ('CONTOUR_SHADE_MAX_LEVEL',        44.)
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',              4.0)
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_METHOD',  'LIST')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_LIST',     CTAB, NLEV)
      CALL PSETC  ('CONTOUR_SHADE',                'ON')      
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'POLYGON_SHADING')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',         'AREA_FILL')
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PCONT
      

C     Plot the title text and the coastlines

      CALL PSETR  ('TEXT_FONT_SIZE', 0.6)
      CALL PSETC  ('TEXT_LINE_1',    '2m Temperature and 30m Wind')
      CALL PTEXT
      CALL PCOAST


C     Now load the wind 30m data

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/wind30m_fc12.grib')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB


C     Define the wind plotting parameters

      CALL PSETR  ('WIND_THINNING_FACTOR',          3.0)
      CALL PWIND


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
