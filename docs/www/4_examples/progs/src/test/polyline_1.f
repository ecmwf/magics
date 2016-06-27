C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM POLYLINE_1
C     Open MAGICS and set the output device

C      PARAMETER (NPOLYS=3)
C      DIMENSION  RVALS (NPOLYS)
C      DATA       RVALS /10., 15., 20./
C
C      PARAMETER (NPOINTS=12+NPOLYS-1)
C      DIMENSION  RX (NPOINTS)
C      DATA       RX / 0., 10., 8.,  0.,  -999., 
C     +               20., 13., 5.,  20., -999.,
C     +               30., 35., 40., 30.  /
C      DIMENSION  RY (NPOINTS)
C      DATA       RY / 0.,  5., 14.,  0., -999.,
C     +                3., 10.,  5.,  3., -999.,
C     +               15., 20., 10., 15. /



      CALL POPEN
      CALL PARSE_COMMAND_LINE ('polyline_1')


      CALL PSETC('LEGEND_TITLE',        'on')
      CALL PSETC('LEGEND_TITLE_TEXT',   'My legend title')
      CALL PSETC('LEGEND_DISPLAY_TYPE', 'continuous')
      CALL PSETC('LEGEND_TEXT_FORMAT',  '(f5.2)')

      
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  77.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   75.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    25.0)
      CALL PSETC ('LEGEND', 'ON')
        
      CALL PSETC ('POLYLINE_INPUT_POSITIONS_FILENAME', 
     +            'data/catchment.pos')
      CALL PSETC ('POLYLINE_INPUT_VALUES_FILENAME', 
     +            'data/catchment.val')
      CALL PSETR ('POLYLINE_INPUT_BREAK_INDICATOR', -999.)
      CALL PSETC ('POLYLINE_SHADE',                  'ON')
      CALL PSETC ('POLYLINE_SHADE_COLOUR_METHOD',    'CALCULATE')
      CALL PSETC ('POLYLINE_SHADE_MAX_LEVEL_COLOUR', 'RED')
      CALL PSETC ('POLYLINE_SHADE_MIN_LEVEL_COLOUR', 'BLUE')
      CALL PSETC ('POLYLINE_SHADE_COLOUR_DIRECTION', 'CLOCKWISE')
      CALL PLINE
      CALL PCOAST

      CALL PNEW  ('PAGE')
      CALL PSETC ('LEGEND_BOX_MODE', 'POSITIONAL')
      CALL PSETR ('LEGEND_BOX_X_POSITION', 25.)
      CALL PSETR ('LEGEND_BOX_Y_POSITION', 7.)
      CALL PSETR ('LEGEND_BOX_X_LENGTH', 5.)
      CALL PSETR ('LEGEND_BOX_Y_LENGTH', 10.)
      CALL PSETC ('LEGEND_BOX_BLANKING', 'ON')
      CALL PSETC ('LEGEND_BOX_BORDER', 'ON')
      CALL PSETR ('POLYLINE_SHADE_MAX_LEVEL', 8.)
      CALL PSETR ('POLYLINE_SHADE_MIN_LEVEL', 4.)
      CALL PSETC ('POLYLINE_SHADE_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR ('POLYLINE_INTERVAL', 0.5)
      CALL PLINE
      CALL PCOAST

      CALL PCLOSE






C      CALL PRESET ('POLYLINE_INPUT_LONGITUDES')
C      CALL PRESET ('POLYLINE_INPUT_LATITUDES')
C      CALL PRESET ('POLYLINE_INPUT_VALUES')
C
C
C
C      CALL PSET1R ('POLYLINE_INPUT_LONGITUDES', RX,    NPOINTS)
C      CALL PSET1R ('POLYLINE_INPUT_LATITUDES',  RY,    NPOINTS)
C      CALL PSET1R ('POLYLINE_INPUT_VALUES',     RVALS, NPOLYS)
C      CALL PSETR  ('POLYLINE_INPUT_MISSING_INDICATOR', -999.)
C
C      call psetc('polyline_shade', 'on')
C      call psetc('polyline_shade_colour_method',    'calculate')
C      call psetc('polyline_shade_max_level_colour', 'red')
C      call psetc('polyline_shade_min_level_colour', 'blue')
C      call psetc('polyline_shade_colour_direction', 'clockwise')
C
C      CALL PSETC ('POLYLINE_LINE_COLOUR', 'RED')
C
C      CALL PLINE



C     Close

      STOP
      END





#include "parse_command_line.h"



