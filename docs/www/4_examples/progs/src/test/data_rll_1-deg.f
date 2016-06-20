C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM DATARLL1DEG

C     Tests T1279 data in full resolution (0.??? degree grid)

C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('data_rll_1-deg')


C     Set up the coastline attributes and plot

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY') 
      CALL PCOAST


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',
     +            'data/prob_icefreesee_0101.grib')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
      CALL PGRIB
      

C     Draw the contour     

c      CALL PSETR  ('CONTOUR_HILO_REDUCTION_RADIUS', 90.0)
c      CALL PSETR  ('CONTOUR_HILO_SUPPRESS_RADIUS',  90.0)
      CALL PSETC  ('CONTOUR_HILO',                   'OFF')
      CALL PCONT


C     Set up and plot the title text

      CALL PTEXT



C     New page, this time zoomed in

      CALL PNEW ('PAGE')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     -37.89)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',      8.83)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    -24.41)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',    26.39)

      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE', 'BOTH')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',  'ON')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 1)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 1)
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',        0.3)
      CALL PSETR ('CONTOUR_GRID_VALUE_MARKER_HEIGHT', 0.2)
      CALL PCONT

      CALL PTEXT

      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',  0.5)    
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT', 0.5)    
      CALL PCOAST



C     Whole area again, this time with cell shading

      CALL PNEW ('PAGE')

      CALL PRESET ('SUBPAGE_LOWER_LEFT_LATITUDE')
      CALL PRESET ('SUBPAGE_LOWER_LEFT_LONGITUDE')
      CALL PRESET ('SUBPAGE_UPPER_RIGHT_LATITUDE')
      CALL PRESET ('SUBPAGE_UPPER_RIGHT_LONGITUDE')

      CALL PSETC  ('CONTOUR',                        'OFF')
      CALL PSETC  ('CONTOUR_SHADE',                  'ON')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE',   'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',                1.0)
      CALL PSETC  ('CONTOUR_SHADE_MIN_LEVEL_COLOUR', 'BLUE')
      CALL PSETC  ('CONTOUR_SHADE_MAX_LEVEL_COLOUR', 'ROSE')
      CALL PSETC  ('CONTOUR_SHADE_COLOUR_DIRECTION', 'CLOCKWISE')
      CALL PSETC  ('CONTOUR_SHADE_METHOD',           'AREA_FILL')
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',        'CELL_SHADING')
c      CALL PSETR ('CONTOUR_SHADE_CELL_RESOLUTION', 1.0)
      CALL PSETC  ('CONTOUR_HILO',                   'OFF')
      CALL PRESET ('CONTOUR_GRID_VALUE_PLOT')
      CALL PCONT

      CALL PTEXT

      CALL PRESET ('MAP_GRID_LATITUDE_INCREMENT')    
      CALL PRESET ('MAP_GRID_LONGITUDE_INCREMENT')    
      CALL PCOAST


C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
