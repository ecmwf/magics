      PROGRAM DATAINVALIDVALUES

C     The input GRIB file has its missing value indicator set to zero
C     so we should not contour these parts. For convenience, it is the
C     sea points that have been invalidated in this way.



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_invalid_values')


C     Set up the geographical area

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    39.95)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -21.95)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   70.21)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  20.0)



C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/t2m_masked.grib')
C      CALL PSETR ('GRIB_MISSING_VALUE_INDICATOR', 0.0)
      CALL PGRIB



C     Set up and plot the coastlines

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
      CALL PCOAST
      


C     Define and plot the contours

      CALL PSETC ('LEGEND',                   'ON')
      CALL PSETR ('CONTOUR_HILO_SUPPRESS_RADIUS',  50.0)
c      CALL PSETC ('CONTOUR_SHADE',            'ON')
      CALL PSETC ('CONTOUR_SHADE_TECHNIQUE',  'POLYGON_SHADING')
      CALL PSETC ('CONTOUR_SHADE_METHOD',     'AREA_FILL')
      CALL PSETC ('CONTOUR_SHADE_COLOUR_DIRECTION', 'CLOCKWISE')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE',   'BOTH')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',        'ON')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 20)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 20)
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',        0.3)
      CALL PSETR ('CONTOUR_GRID_VALUE_MARKER_HEIGHT', 0.2)
      CALL PSETC ('CONTOUR_GRID_VALUE_MARKER_COLOUR', 'BLACK')
      CALL PSETC ('CONTOUR_GRID_VALUE_FORMAT', '(I3)')
      CALL PCONT



C     Set up the text attributes

      CALL PSETI ('TEXT_LINE_COUNT', 3)
      CALL PSETC ('TEXT_LINE_1', '2 Meter Temperature, 15 Aug 2006')
      CALL PSETC ('TEXT_LINE_2', 'Only land values are valid')
      CALL PSETC ('TEXT_LINE_3', 'Every 20th grid value is plotted')
      CALL PTEXT


C     Another page, this time loading a lower-resolution version
C     of the data so that we use Akima interpolation.

      CALL PNEW ('SUPER_PAGE')

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/t2m_masked_1_5.grib')
      CALL PGRIB

      CALL PCOAST

      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 1)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 1)
      CALL PCONT

      CALL PSETI ('TEXT_LINE_COUNT', 3)
      CALL PSETC ('TEXT_LINE_1', '2 Meter Temperature, 15 Aug 2006')
      CALL PSETC ('TEXT_LINE_2', 'Only land values are valid')
      CALL PSETC ('TEXT_LINE_3', 'Every grid value is plotted')
      CALL PTEXT

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
