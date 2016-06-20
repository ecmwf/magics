C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONTHATCHSHADE

C     This program demonstrates hatch shading.


      PARAMETER (NLEV=11)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /48000, 49000., 50000., 51000., 52000., 53000.,
     +                 54000., 55000., 56000., 57000., 58000./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_hatchshade_ex')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
      CALL PSETC ('GRIB_SCALING',         'OFF')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('LEGEND',                       'ON')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      
      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC  ('CONTOUR_SHADE',            'ON')      
      call PSETC  ('CONTOUR_SHADE_TECHNIQUE',  'POLYGON_SHADING')
      call PSETC  ('CONTOUR_SHADE_METHOD',     'HATCH')
      CALL PCONT
      

C     Set up and plot the title text

      CALL PSETC ('TEXT_LINE_1', 'Contours with hatch shading')
      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT


C     Plot the coastlines

      CALL PCOAST


C     ------------------------------------------------------------
C     New page - this time with a smaller area
C     ------------------------------------------------------------

      CALL PNEW ('SUPER_PAGE')


C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    30.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -30.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   65.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  70.0)


C     Plot everything

      CALL PTEXT
      CALL PCOAST
      CALL PCONT




      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
