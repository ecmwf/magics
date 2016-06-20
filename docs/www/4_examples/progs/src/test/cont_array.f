C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONTARRAY

C     Demonstrates passing in-memory arrays of data to MAGICS
C     for contouring.

      PARAMETER (NLON=240, NLAT=121)
      DIMENSION FIELD (NLON,NLAT)
      REAL      LAT_STEP,  LON_STEP
      REAL      LAT_FIRST, LON_FIRST
      INTEGER   LAT, LON


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_array')



C     Set the field data

      LAT_STEP  = -1.5
      LON_STEP  =  1.5
      LAT_FIRST = 90.0
      LON_FIRST =  0.0
      LAT_LAST  = LAT_FIRST + (LAT_STEP * NLAT)


      CALL PSETR ('INPUT_FIELD_INITIAL_LATITUDE',  LAT_FIRST)             
      CALL PSETR ('INPUT_FIELD_INITIAL_LONGITUDE', LON_FIRST)             
      CALL PSETR ('INPUT_FIELD_LATITUDE_STEP',     LAT_STEP)                
      CALL PSETR ('INPUT_FIELD_LONGITUDE_STEP',    LON_STEP)                


      DO 10 I = 1, NLAT
        LAT = LAT_FIRST + (I * LAT_STEP)
        DO 20 J = 1, NLON
          LON = LON_FIRST + (J * LON_STEP)
          FIELD(J,I) = LON + LAT
  20    CONTINUE
  10  CONTINUE




C     Pass the data to MAGICS

      CALL PSET2R ('INPUT_FIELD', FIELD, NLON, NLAT)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Define the contour     

      CALL PSETC ('CONTOUR',                  'OFF')
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT',         'ON')
      CALL PSETC ('CONTOUR_GRID_VALUE_PLOT_TYPE',    'VALUE')
      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 8)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 8)
      CALL PCONT


C     Plot the coastlines and text

      CALL PSETC ('TEXT_LINE_1',  'Grid values from array (lat + lon)')
      CALL PTEXT
      CALL PCOAST




C     ---------------------------------------------
C     Start a new page, this time showing a subarea
C     ---------------------------------------------


      CALL PNEW  ('SUPER_PAGE')


C     Define map area and projection

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -10.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',   -10.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE', 170.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   85.0)


C     Adjust the grid value plotting

      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 4)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 4)
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',        0.3)


      CALL PCONT
      CALL PTEXT
      CALL PCOAST



C     ---------------------------------------------
C     Start a new page, this time showing a subarea
C     ---------------------------------------------


      CALL PNEW  ('SUPER_PAGE')


C     Define map area and projection

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -10.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    50.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   5.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   60.0)


C     Adjust the grid value plotting

      CALL PSETI ('CONTOUR_GRID_VALUE_LAT_FREQUENCY', 1)
      CALL PSETI ('CONTOUR_GRID_VALUE_LON_FREQUENCY', 1)
      CALL PSETR ('CONTOUR_GRID_VALUE_HEIGHT',        0.3)


      CALL PCONT
      CALL PTEXT
      CALL PCOAST

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
