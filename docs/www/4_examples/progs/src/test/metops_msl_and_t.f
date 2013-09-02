      PROGRAM METOPS_MSL_AND_T


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('metops_msl_and_t')


C   set up the page

      CALL PSETR ('SUPER_PAGE_X_LENGTH',   42.0)
      CALL PSETR ('SUPER_PAGE_Y_LENGTH',   29.7)
      CALL PSETR ('PAGE_X_LENGTH',         42.0)
      CALL PSETR ('PAGE_Y_LENGTH',         29.7)
      CALL PSETR ('SUBPAGE_X_POSITION',     1.0)
      CALL PSETR ('SUBPAGE_Y_POSITION',     3.0)
      CALL PSETR ('SUBPAGE_X_LENGTH',      40.0)
      CALL PSETR ('SUBPAGE_Y_LENGTH',      25.0)
      CALL PSETC ('PS_DEVICE',            'ps_a3')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     6.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -49.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   39.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', 105.0)



C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
      CALL PSETC ('MAP_GRID_COLOUR',      'BLACK')     


C     First, load and plot the MSL data

C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/p12pt850_MSL.grib')
      CALL PGRIB


C     Define and plot the contour     

c      CALL PSETC  ('CONTOUR_METHOD',     'AKIMA760')
c      CALL PSETR  ('AKIMA_RESOLUTION_X',  1.5)
c      CALL PSETR  ('AKIMA_RESOLUTION_Y',  1.5)


      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETI  ('CONTOUR_LINE_THICKNESS',        4)
      CALL PSETR  ('CONTOUR_INTERVAL',              5.0)
      CALL PSETC  ('CONTOUR_HILO_QUALITY',         'HIGH')
c      CALL PSETR  ('CONTOUR_HILO_SUPPRESS_RADIUS',  50.0)
      CALL PSETI  ('contour_hilo_window_size',      25)
      CALL PSETC  ('CONTOUR_HI_COLOUR',            'RED')
      CALL PSETC  ('CONTOUR_LO_COLOUR',            'RED')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'OFF')
      CALL PCONT



C     Now load and plot the T data

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/p12pt850_T.grib')
      CALL PGRIB

      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETC  ('CONTOUR_LINE_STYLE',           'DOT')
      CALL PSETI  ('CONTOUR_LINE_THICKNESS',        2)
      CALL PSETR  ('CONTOUR_INTERVAL',              2.0)
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'ON')
      CALL PSETI  ('CONTOUR_HIGHLIGHT_FREQUENCY',   100)
      CALL PSETC  ('CONTOUR_HIGHLIGHT_STYLE',      'DASH')
      CALL PSETI  ('CONTOUR_HIGHLIGHT_THICKNESS',   4)
      CALL PCONT


C     Plot the title text at the bottom

      CALL PSETC ('TEXT_MODE',          'POSITIONAL')
      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_2', 'MSL PRESSURE AND 850 hPa TEMPERATURE')
      CALL PSETR ('TEXT_BOX_X_POSITION',  1.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',  0.5)
      CALL PSETR ('TEXT_BOX_X_LENGTH',   40.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    2.3)
      CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
      CALL PSETC ('TEXT_QUALITY',       'HIGH')
      CALL PSETC ('TEXT_BORDER',        'OFF')
      CALL PTEXT



C     Plot the coastlines

      CALL PCOAST







C     ----------------------------------------------------------
C     New page, this time with fields from a different time/date
C     ----------------------------------------------------------

      CALL PNEW ('SUPER_PAGE')


C     Resest the contouring attributes that were set up for T

      CALL PRESET  ('CONTOUR_LINE_STYLE')
      CALL PRESET  ('CONTOUR_LINE_THICKNESS')
      CALL PRESET  ('CONTOUR_INTERVAL')
      CALL PRESET  ('CONTOUR_HILO')
      CALL PRESET  ('CONTOUR_HIGHLIGHT')
      CALL PRESET  ('CONTOUR_HIGHLIGHT_FREQUENCY')
      CALL PRESET  ('CONTOUR_HIGHLIGHT_STYLE')
      CALL PRESET  ('CONTOUR_HIGHLIGHT_THICKNESS')
      


C     First, load and plot the MSL data

C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     x 'data/p12pt850_MSL_+60_025.grib')
      CALL PGRIB


C     Define and plot the contour     

c      CALL PSETC  ('CONTOUR_METHOD',     'AKIMA760')
c      CALL PSETR  ('AKIMA_RESOLUTION_X',  0.9)
c      CALL PSETR  ('AKIMA_RESOLUTION_Y',  0.9)
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETI  ('CONTOUR_LINE_THICKNESS',        4)
      CALL PSETR  ('CONTOUR_INTERVAL',              5.0)
      CALL PSETC  ('CONTOUR_HILO_QUALITY',         'HIGH')
c      CALL PSETR  ('CONTOUR_HILO_SUPPRESS_RADIUS',  50.0)
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'OFF')
      CALL PCONT



C     Now load and plot the T data

      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     x 'data/p12pt850_T_+60_025.grib')
      CALL PGRIB

      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETC  ('CONTOUR_LINE_STYLE',           'DOT')
      CALL PSETI  ('CONTOUR_LINE_THICKNESS',        2)
      CALL PSETR  ('CONTOUR_INTERVAL',              2.0)
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'ON')
      CALL PSETI  ('CONTOUR_HIGHLIGHT_FREQUENCY',   100)
      CALL PSETC  ('CONTOUR_HIGHLIGHT_STYLE',      'DASH')
      CALL PSETI  ('CONTOUR_HIGHLIGHT_THICKNESS',   4)
      CALL PCONT


C     Plot the title text at the bottom

      CALL PSETC ('TEXT_MODE',          'POSITIONAL')
      CALL PSETI ('TEXT_LINE_COUNT', 3)
      CALL PSETC ('TEXT_LINE_2', 'MSL PRESSURE AND 850 hPa TEMPERATURE')
      CALL PSETC ('TEXT_LINE_3', 'FORECAST +60')
      CALL PSETR ('TEXT_BOX_X_POSITION',  1.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',  0.5)
      CALL PSETR ('TEXT_BOX_X_LENGTH',   40.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    2.3)
      CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
      CALL PSETC ('TEXT_QUALITY',       'HIGH')
      CALL PSETC ('TEXT_BORDER',        'OFF')
      CALL PTEXT



C     Plot the coastlines

      CALL PCOAST


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
