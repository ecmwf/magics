C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONTAUTO

C     This program demonstrates magics contouring facilities.
C     Set to 'AUTOMATIC', Magics++ attempts to find the 'best'
C     contouring parameters for the trade-off between quality and speed. 


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_auto_t799')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')


C     Pass the data to MAGICS
 
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/t799_ll.grib')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('CONTOUR_LINE_COLOUR',          'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'OFF')
      CALL PSETC  ('CONTOUR_HILO',                 'OFF')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',              2.5)
      CALL PCONT
      

C     Set up and plot the title text

      CALL PSETC ('TEXT_LINE_1', 'Contours with automatic parameters')
      CALL PSETI ('TEXT_LINE_COUNT', 1)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      CALL PTEXT


C     Plot the coastlines

      CALL PCOAST



C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -68.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -135.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    68.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  135.0)

      CALL PSETC ('TEXT_LINE_1', 
     x            'Automatic Contouring, SWNE = -68.0, -135, 68, 135')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',   -45.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -90.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   45.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  90.0)

      CALL PSETC ('TEXT_LINE_1', 
     x            'Automatic Contouring, SWNE = -45.0, -90, 45, 90')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -20.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -45.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    20.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   45.0)

      CALL PSETC ('TEXT_LINE_1', 
     x            'Automatic Contouring, SWNE = -20, -45, 20, 45')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',      -20.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',     -25.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',       0.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',      0.0)

      CALL PSETC ('TEXT_LINE_1', 
     x            'Automatic Contouring, SWNE = -20, -25, 0, 0')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST




C  --------------------- Cleanup and finish ----------------------

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

