      PROGRAM METOPS_CLOUDCOVER

       PARAMETER (NCOLS=64)
       CHARACTER*25 CTAB
       DIMENSION  CTAB (NCOLS)
       DATA       CTAB /'HSL(0,0,1)',
     +                  'HSL(29,0.14,0.92)',
     +                  'HSL(29,0.29,0.83)',
     +                  'HSL(29,0.43,0.75)',
     +                  'HSL(300,0.08,0.92)',
     +                  'HSL(360,0.16,0.84)',
     +                  'HSL(13,0.3,0.75)',
     +                  'HSL(18,0.44,0.67)',
     +                  'HSL(300,0.16,0.83)',
     +                  'HSL(340,0.22,0.75)',
     +                  'HSL(360,0.34,0.67)',
     +                  'HSL(8,0.47,0.58)',
     +                  'HSL(300,0.24,0.75)',
     +                  'HSL(330,0.28,0.67)',
     +                  'HSL(349,0.38,0.58)',
     +                  'HSL(360,0.5,0.5)',
     +                  'HSL(180,0.17,0.92)',
     +                  'HSL(120,0.08,0.84)',
     +                  'HSL(57,0.17,0.75)',
     +                  'HSL(44,0.3,0.67)',
     +                  'HSL(209,0.14,0.84)',
     +                  'HSL(187,0,0.75)',
     +                  'HSL(29,0.15,0.67)',
     +                  'HSL(29,0.29,0.59)',
     +                  'HSL(239,0.16,0.75)',
     +                  'HSL(299,0.08,0.67)',
     +                  'HSL(360,0.17,0.58)',
     +                  'HSL(13,0.3,0.5)',
     +                  'HSL(258,0.21,0.67)',
     +                  'HSL(299,0.16,0.59)',
     +                  'HSL(341,0.22,0.5)',
     +                  'HSL(360,0.33,0.42)',
     +                  'HSL(180,0.34,0.83)',
     +                  'HSL(161,0.22,0.75)',
     +                  'HSL(120,0.16,0.67)',
     +                  'HSL(78,0.21,0.58)',
     +                  'HSL(193,0.3,0.75)',
     +                  'HSL(180,0.17,0.67)',
     +                  'HSL(120,0.08,0.58)',
     +                  'HSL(59,0.16,0.5)',
     +                  'HSL(209,0.29,0.67)',
     +                  'HSL(209,0.15,0.58)',
     +                  'HSL(217,0,0.5)',
     +                  'HSL(29,0.14,0.42)',
     +                  'HSL(224,0.3,0.58)',
     +                  'HSL(237,0.17,0.5)',
     +                  'HSL(299,0.08,0.42)',
     +                  'HSL(360,0.16,0.33)',
     +                  'HSL(180,0.5,0.75)',
     +                  'HSL(169,0.38,0.67)',
     +                  'HSL(150,0.28,0.58)',
     +                  'HSL(120,0.24,0.5)',
     +                  'HSL(188,0.47,0.67)',
     +                  'HSL(180,0.34,0.59)',
     +                  'HSL(160,0.22,0.5)',
     +                  'HSL(120,0.16,0.42)',
     +                  'HSL(198,0.44,0.58)',
     +                  'HSL(193,0.3,0.5)',
     +                  'HSL(180,0.17,0.42)',
     +                  'HSL(120,0.08,0.33)',
     +                  'HSL(209,0.43,0.5)',
     +                  'HSL(209,0.29,0.42)',
     +                  'HSL(209,0.14,0.33)',
     +                  'HSL(191,0,0.25)'/
       PARAMETER (NLEV=65)
       DIMENSION  RLEV (NLEV)
       DATA       RLEV /-0.5,
     +                  0.5,
     +                  1.5,
     +                  2.5,
     +                  3.5,
     +                  4.5,
     +                  5.5,
     +                  6.5,
     +                  7.5,
     +                  8.5,
     +                  9.5,
     +                  10.5,
     +                  11.5,
     +                  12.5,
     +                  13.5,
     +                  14.5,
     +                  15.5,
     +                  16.5,
     +                  17.5,
     +                  18.5,
     +                  19.5,
     +                  20.5,
     +                  21.5,
     +                  22.5,
     +                  23.5,
     +                  24.5,
     +                  25.5,
     +                  26.5,
     +                  27.5,
     +                  28.5,
     +                  29.5,
     +                  30.5,
     +                  31.5,
     +                  32.5,
     +                  33.5,
     +                  34.5,
     +                  35.5,
     +                  36.5,
     +                  37.5,
     +                  38.5,
     +                  39.5,
     +                  40.5,
     +                  41.5,
     +                  42.5,
     +                  43.5,
     +                  44.5,
     +                  45.5,
     +                  46.5,
     +                  47.5,
     +                  48.5,
     +                  49.5,
     +                  50.5,
     +                  51.5,
     +                  52.5,
     +                  53.5,
     +                  54.5,
     +                  55.5,
     +                  56.5,
     +                  57.5,
     +                  58.5,
     +                  59.5,
     +                  60.5,
     +                  61.5,
     +                  62.5,
     +                  63.5/
      CHARACTER TITLE*256



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('metops_cloudcover')


C     Area specification (SOUTH, WEST, NORTH, EAST )

C      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    23.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -33.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   51.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  72.0)




C     First, load and plot the temperature data, shaded

C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/cloudcover.grib')
      CALL PGRIB
      

C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')     


C     Define the contour     

      CALL PSETC  ('LEGEND',                       'OFF')
      CALL PSETC  ('CONTOUR',                      'OFF')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      
      CALL PSETC  ('CONTOUR_SHADE',                'ON')      
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'CELL_SHADING')
c      CALL PSETR ('CONTOUR_SHADE_CELL_RESOLUTION', 1.0)
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_METHOD',  'LIST')
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_LIST',     CTAB, NCOLS)
      CALL PSETC  ('CONTOUR_SHADE_METHOD',         'AREA_FILL')
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PCONT
      

C     Plot the title text and the coastlines

      TITLE = 'Z700 \\\CL' //  
     +        CTAB(4)  // '\\\Low,\\\CL' //
     +        CTAB(16) // '\\\ L+M,\\\CL' //
     +        CTAB(13) // '\\\ Medium,\\\CL' //
     +        CTAB(61) // '\\\ M+H,\\\CL' //
     +        CTAB(49) // '\\\ High,\\\CL' //
     +        CTAB(52) // '\\\ H+L,\\\CLBLACK\\\ H+M+L' //
     +        '\\\CLR\\\ clouds'


      CALL PSETR  ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.40)
      CALL PSETC  ('TEXT_LINE_1',      TITLE)
      CALL PTEXT
      CALL PCOAST


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
