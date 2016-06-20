C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM LAYOUTMANUAL2
      
C     This program demonstrates contour attributes in Magics++.
C     We use the manual layout facilities to create a set of plots
C     stacked vertically from the bottom of the page.


      REAL SUPER_W
      REAL SUPER_H
      REAL Y_INCREMENT
      INTEGER NUM_PLOTS_PER_PAGE



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('layout_manual_2')


C     Set up the main page dimensions

      SUPER_W = 21.0
      SUPER_H = 29.7
      

      CALL PSETC ('LAYOUT',             'POSITIONAL')
      CALL PSETC ('PAGE_FRAME',         'ON')

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', SUPER_H)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', SUPER_W)

      CALL PSETR ('PAGE_X_LENGTH',   SUPER_W * 0.95)
      CALL PSETR ('PAGE_Y_LENGTH',   12.0)
      CALL PSETR ('PAGE_Y_POSITION', 0.0)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     



C     Define the geographical area for our first page

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',   -20.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -100.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  100.0)


C     Pass the data to MAGICS
 
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB


C     Set up our generic text attributes

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Default Contours')
      CALL PTEXT
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', 13.0)
      CALL PSETC ('TEXT_LINE_1', 'Dot Contours')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LINE_STYLE',  'DOT')     
      CALL PSETC ('CONTOUR_LINE_COLOUR', 'BLUE')     
      CALL PCONT


CC     Page 3
C
C      CALL PNEW  ('PAGE')
C      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 2)
C      CALL PSETC ('TEXT_LINE_1', 'Dash Contours')
C      CALL PTEXT
C      CALL PSETC ('CONTOUR_LINE_STYLE',  'DASH')     
C      CALL PSETC ('CONTOUR_LINE_COLOUR', 'BURGUNDY')     
C      CALL PCONT
C
C
CC     Page 4
C
C      CALL PNEW  ('PAGE')
C      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 3)
C      CALL PSETC ('TEXT_LINE_1', 'Solid, Thick Contours')
C      CALL PTEXT
C      CALL PSETC ('CONTOUR_LINE_STYLE',      'SOLID')     
C      CALL PSETC ('CONTOUR_LINE_COLOUR',     'GREEN')     
C      CALL PSETI ('CONTOUR_LINE_THICKNESS',   4)     
C      CALL PCONT




C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

