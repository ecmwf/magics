C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM METOPS_P12ZT500


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('metops_p12zt500')


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


C     First, load and plot the Z data

C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/p12zt500_Z.grib')
      CALL PGRIB


C     Define and plot the contour     

      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETI  ('CONTOUR_LINE_THICKNESS',        4)
      CALL PSETR  ('CONTOUR_INTERVAL',              4.0)
      CALL PSETC  ('CONTOUR_HILO_QUALITY',         'HIGH')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'OFF')
      CALL PCONT



C     Now load and plot the T data

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/p12zt500_T.grib')
      CALL PGRIB

      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETC  ('CONTOUR_LINE_STYLE',           'DOT')
      CALL PSETI  ('CONTOUR_LINE_THICKNESS',        2)
      CALL PSETR  ('CONTOUR_INTERVAL',              2.0)
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'OFF')
      CALL PCONT


C     Plot the title text at the bottom

      CALL PSETC ('TEXT_MODE',          'POSITIONAL')
      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_2', '500 hPa HEIGHT AND TEMPERATURE')
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
