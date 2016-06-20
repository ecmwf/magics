C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM COASTATTRIBUTES
      
C     This program demonstrates coastline attributes in Magics++.
C     We use the automatic layout facilities to create a set of plots
C     stacked vertically from the top of the page.


      REAL SUPER_W
      REAL SUPER_H
      INTEGER NUM_PLOTS_PER_PAGE



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('coast_attributes_ex')


C     Set up the main page dimensions

      SUPER_W = 21.0
      SUPER_H = 39.7
      NUM_PLOTS_PER_PAGE = 4
      

      CALL PSETC ('LAYOUT',             'AUTOMATIC')
      CALL PSETC ('PLOT_START',         'TOP')
      CALL PSETC ('PLOT_DIRECTION',     'VERTICAL')

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', SUPER_H)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', SUPER_W)

      CALL PSETR ('PAGE_Y_LENGTH', SUPER_H / (NUM_PLOTS_PER_PAGE*1.1))


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     



C     Define the geographical area for our first page

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    45.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -18.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   5.0)



C     Set up our generic text attributes

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Solid Coastlines')
      CALL PTEXT
      CALL PSETC ('MAP_COASTLINE_STYLE',  'SOLID')     
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'RED')     
      CALL PCOAST


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Dot Coastlines')
      CALL PTEXT
      CALL PSETC ('MAP_COASTLINE_STYLE',  'DOT')     
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLUE')     
      CALL PCOAST


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Dash Coastlines')
      CALL PTEXT
      CALL PSETC ('MAP_COASTLINE_STYLE',  'DASH')     
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'BURGUNDY')     
      CALL PCOAST


C     Page 4

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Solid, thick Coastlines')
      CALL PTEXT
      CALL PSETC ('MAP_COASTLINE_STYLE',     'SOLID')     
      CALL PSETC ('MAP_COASTLINE_COLOUR',    'GREEN')     
      CALL PSETI ('MAP_COASTLINE_THICKNESS',  4)     
      CALL PCOAST



C     ------------------------------------------
C     Start a new super page
C     Here we play with the grid line attributes
C     ------------------------------------------

      CALL PNEW  ('SUPER_PAGE')

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -45.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -80.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   80.0)


      CALL PSETC ('MAP_COASTLINE',  'OFF')     


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Solid Gridlines')
      CALL PTEXT
      CALL PSETC ('MAP_GRID_LINE_STYLE', 'SOLID')     
      CALL PSETC ('MAP_GRID_COLOUR',     'RED')     
      CALL PCOAST


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Dot Gridlines')
      CALL PTEXT
      CALL PSETC ('MAP_GRID_LINE_STYLE', 'DOT')     
      CALL PSETC ('MAP_GRID_COLOUR',     'BLUE')     
      CALL PCOAST


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Dash Gridlines')
      CALL PTEXT
      CALL PSETC ('MAP_GRID_LINE_STYLE', 'DASH')     
      CALL PSETC ('MAP_GRID_COLOUR',     'BURGUNDY')     
      CALL PCOAST


C     Page 4

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Solid, thick Gridlines')
      CALL PTEXT
      CALL PSETC ('MAP_GRID_LINE_STYLE', 'SOLID')     
      CALL PSETC ('MAP_GRID_COLOUR',     'GREEN')     
      CALL PSETI ('MAP_GRID_THICKNESS',  4)     
      CALL PCOAST



C     ------------------------------------------
C     Start a new super page
C     Here we play with the map label attributes
C     ------------------------------------------

      CALL PNEW  ('SUPER_PAGE')

C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Label every 2/4th grid line')
      CALL PTEXT
      CALL PSETI ('MAP_GRID_THICKNESS',            1)     
      CALL PSETI ('MAP_LABEL_LONGITUDE_FREQUENCY', 4)     
      CALL PSETI ('MAP_LABEL_LATITUDE_FREQUENCY',  2)     
      CALL PCOAST


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Large Red labels')
      CALL PTEXT
      CALL PSETR ('MAP_LABEL_HEIGHT', 0.35)     
      CALL PSETC ('MAP_LABEL_COLOUR', 'RED')     
      CALL PCOAST


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Small labels, all lines labelled')
      CALL PTEXT
      CALL PSETI ('MAP_LABEL_LONGITUDE_FREQUENCY', 1)
      CALL PSETI ('MAP_LABEL_LATITUDE_FREQUENCY',  1)
      CALL PSETR ('MAP_LABEL_HEIGHT', 0.1)     
      CALL PCOAST



C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

