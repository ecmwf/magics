C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM LAYOUTAUTO

C     This program demonstrates the automatic layout facilities in MAGICS.
C     We generate two plots on the same page.


      PARAMETER (NLEV=6)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /54500.,55000.,55500.,56000.,56500.,57000./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('layout_auto_ex')

      CALL PSETC ('TEXT_MODE',     'POSITIONAL')
      CALL PSETC ('TEXT_LINE_1', 'Positional Text 1')



C     Set up the main page dimensions

      CALL PSETC ('LAYOUT',             'AUTOMATIC')
      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 21.0)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 29.7)
      CALL PSETR ('PAGE_X_LENGTH',       12.0)
      CALL PSETR ('PAGE_Y_LENGTH',       15.0)
      CALL PSETR ('PAGE_X_POSITION',      1.5)
      CALL PSETR ('PAGE_Y_POSITION',      1.5)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Define the geographical area for our first page

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    45.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -18.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   5.0)


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',         'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME',    'data/z500.grb')
#if defined (MAGPLUS)
      CALL PSETC ('GRIB_AUTOMATIC_SCALING', 'OFF')
#else
      CALL PSETC ('GRIB_SCALING', 'OFF')
#endif
      CALL PSETC ('GRIB_SPECIFICATION',      'OFF')
      CALL PSETC ('GRIB_SUBAREA_EXTRACTION', 'OFF')
      CALL PGRIB


C     Define the contour     

      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R('CONTOUR_LEVEL_LIST',            RLEV, NLEV)


C     Set up our generic text attributes

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')


C     Page 1

      CALL PSETR ('SUBPAGE_X_LENGTH',       5.0)
      CALL PSETR ('SUBPAGE_Y_LENGTH',       12.0)
      CALL PSETR ('SUBPAGE_X_POSITION',      1.5)
      CALL PSETR ('SUBPAGE_Y_POSITION',      1.5)
      CALL PSETC ('TEXT_LINE_1', 'Page 1')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST


      CALL PSETR ('TEXT_BOX_X_POSITION', 2.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION', 1.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    4.0)
      CALL PSETC ('TEXT_LINE_1', 'text 1')
      CALL PTEXT





C     Page 2

      CALL PNEW  ('PAGE')

      CALL PSETR ('SUBPAGE_X_LENGTH',        6.0)
      CALL PSETR ('SUBPAGE_Y_LENGTH',       12.0)
      CALL PSETR ('SUBPAGE_X_POSITION',      6.5)
      CALL PSETR ('SUBPAGE_Y_POSITION',      1.5)

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    35.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   50.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  20.0)

      CALL PSETC ('TEXT_LINE_1', 'Page 2')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST


      CALL PSETR ('TEXT_BOX_X_POSITION', 2.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION', 1.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    4.0)
      CALL PSETC ('TEXT_LINE_1', 'text 2')
      CALL PTEXT


C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

