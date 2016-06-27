C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM WIND

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('wind_test')


C     Set up all the parameters we'll use in all the examples

      CALL PSETC ('MAP_COASTLINE_COLOUR',            'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',                 'TAN')
      CALL PSETC ('MAP_LABEL_COLOUR',                'TAN')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    62.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   0.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   70.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  15.0)

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/windspeed.grb')
      CALL PGRIB


C     -----------------------------------------------------------
C     General setup, and plotting of everything (all grid points)
C     -----------------------------------------------------------




C     Show the wind speed as grid points

      CALL PSETC ('CONTOUR',                         'OFF')
      CALL PSETC ('CONTOUR_METHOD',                  'LINEAR')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE',    'BOTH')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',         'ON')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY',  1)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY',  1)
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',         0.3)
      CALL PSETC ('CONTOUR_GRID_VALUE_COLOUR',        'GREY')
      CALL PSETR ('CONTOUR_GRID_VALUE_MARKER_HEIGHT',  0.2)
      CALL PSETC ('CONTOUR_GRID_VALUE_MARKER_COLOUR', 'ROSE')
      CALL PCONT


C     Coastlines...

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PCOAST 


C     Text...

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_LINE_1', 'Wind speed / arrows')
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT


C     Now plot the wind arrows on top

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/uv100.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB

      CALL PSETR ('WIND_THINNING_FACTOR', 1.0)
      CALL PWIND



C     ------------------------------------------------------------
C     New page - set wind plotting paramters to remove some arrows
C     ------------------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/windspeed.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', -1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', -1)
      CALL PGRIB
      CALL PCONT

      CALL PCOAST 

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/uv100.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB

      CALL PSETR ('WIND_ARROW_MAX_SPEED',       11.0)
      CALL PSETR ('WIND_ARROW_MIN_SPEED',        3.0)
      CALL PSETC ('WIND_ARROW_CALM_INDICATOR',  'ON')
      CALL PSETR ('WIND_ARROW_CALM_BELOW',       5.0)
      CALL PSETC ('WIND_ARROW_ORIGIN_POSITION', 'CENTRE')

c      CALL PSETC ('WIND_ARROW_CROSS_BOUNDARY', 'OFF')
      CALL PWIND


      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_1',    'Wind speed / arrows')
      CALL PSETC ('TEXT_LINE_2',    
     x            'Between 3.0 and 11.0. Calm below 5.0')
      CALL PTEXT




C     ------------------------------------------------------------
C     New page - set wind plotting paramters to remove some flags
C     ------------------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/windspeed.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', -1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', -1)
      CALL PGRIB
      CALL PCONT

      CALL PCOAST 

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/uv100.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB

      CALL PSETC ('WIND_FIELD_TYPE',     'FLAGS')
      CALL PSETR ('WIND_FLAG_MAX_SPEED', 10.0)
      CALL PSETR ('WIND_FLAG_MIN_SPEED',  4.0)
      CALL PWIND

      CALL PSETC ('TEXT_LINE_1',    'Wind speed / flags')
      CALL PSETC ('TEXT_LINE_2',    
     x            'Between 4.0 and 10.0. Calm at default (0.5)')
      CALL PTEXT









C     ------------------------------------------------------------
C     New page - set wind plotting paramters to remove some arrows
C     ------------------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/windspeed.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', -1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', -1)
      CALL PGRIB
      CALL PCONT

      CALL PCOAST 

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/uv100.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB

      CALL PSETC ('WIND_FIELD_TYPE',            'ARROWS')
      CALL PSETR ('WIND_ARROW_MAX_SPEED',       11.0)
      CALL PSETR ('WIND_ARROW_MIN_SPEED',        3.0)
      CALL PSETC ('WIND_ARROW_CALM_INDICATOR',  'ON')
      CALL PSETR ('WIND_ARROW_CALM_BELOW',       5.0)
      CALL PSETC ('WIND_ARROW_ORIGIN_POSITION', 'CENTRE')

c      CALL PSETC ('WIND_ARROW_CROSS_BOUNDARY', 'OFF')
      CALL PWIND


      CALL PSETI ('TEXT_LINE_COUNT', 2)
      CALL PSETC ('TEXT_LINE_1',    'Wind speed / arrows')
      CALL PSETC ('TEXT_LINE_2',    
     x            'Between 3.0 and 11.0. Calm below 5.0')
      CALL PTEXT





C     ------------------------------------------------------------
C     New page - start testing the wind flags - do they have the
C     correct number and type of penants?
C     ------------------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')

      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/uv500.grb')
      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
      CALL PGRIB
      CALL PRESET ('GRIB_WIND_POSITION_1')               
      CALL PRESET ('GRIB_WIND_POSITION_2')
      CALL PSETC ('WIND_FIELD_TYPE',      'FLAGS')
      CALL PWIND

      CALL PSETC  ('GRIB_INPUT_FILE_NAME', 'data/windspeed500.grb')
      CALL PGRIB
      CALL PCONT

      CALL PSETI ('TEXT_LINE_COUNT', 1)
      CALL PSETC ('TEXT_LINE_1',    'Wind speed / flags')
      CALL PTEXT




C      CALL PNEW  ('SUPER_PAGE')
Cc      CALL PSETI ('GRIB_FIELD_POSITION', 2)               
C      CALL PGRIB
C      CALL PSETC ('TEXT_LINE_1', 'V field values')
C      CALL PCONT
C      CALL PCOAST 
C      CALL PTEXT


C
C
C      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/windspeed.grb')
C
C
C
CC     Start a new page, this time showing the wind arrows
C
C      CALL PNEW  ('SUPER_PAGE')
C
C      CALL PSETI ('GRIB_WIND_POSITION_1', 1)               
C      CALL PSETI ('GRIB_WIND_POSITION_2', 2)
C      CALL PGRIB
C
C
CC     Plot the wind field using default wind-plotting paramters
C
C      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, no thinning')
C
C      CALL PCOAST 
C      CALL PWIND
C      CALL PTEXT
C
C
C      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/windspeed.grb')
C      CALL PCONT
C
C
C
CC     Start a new page, this time with no thinning
C
C      CALL PNEW  ('SUPER_PAGE')
C      CALL PSETR ('WIND_THINNING_FACTOR', 1.0)
C      CALL PSETC ('TEXT_LINE_1', 'Wind arrows, thinning 1.0')
C      CALL PCOAST 
C      CALL PWIND
C      CALL PTEXT


      CALL PCLOSE
      END


#include "parse_command_line.h"
