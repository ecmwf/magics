C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONTSPLIT

C     A test of Magics++ solid shading, using lots of different areas


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_split')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
        


C     Set up the text attributes

      CALL PSETI ('TEXT_LINE_COUNT', 3)


C     Define the contour attributes

      CALL PSETC  ('LEGEND',                       'ON')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR  ('CONTOUR_REFERENCE_LEVEL',       0.0)
      CALL PSETR  ('CONTOUR_INTERVAL',              2.0)


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/Tdiff.grib')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
c      CALL PSETC ('GRIB_SCALING',         'OFF')
      CALL PGRIB
      



C     Start plotting - one page per geographical area.

C                        LEFT , RIGHT, TOP , BOTTOM, NEWPAGE?
      CALL PSETC ('TEXT_LINE_3', 'Global area')
      CALL CONTOUR_TEST (-180.,  180.,  90.,  -90.,   0)

      CALL PSETC ('TEXT_LINE_3', 'Whole data area')
      CALL CONTOUR_TEST (-60.,  60.,  85.,  15.,      1)


      CALL PCLOSE

      STOP
      END



C    ------------------------------------------------------------
C    CONTOUR_TEST
C    Plots solid shaded contours with coastlines within the given
C    geographical area.
C    ------------------------------------------------------------

      SUBROUTINE CONTOUR_TEST (RLEFT, RIGHT, RTOP, RBOT, INEW)
      
          CHARACTER*80  TITLE
          REAL          RLATINC, RLONGINC


C         Optionally start a new page

          IF (INEW == 1) THEN
            CALL PNEW  ('SUPER_PAGE')
          END IF


C         Set up the geographical area

          CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    RBOT)
          CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   RLEFT)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   RTOP)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  RIGHT)


C         Calculate the best lat/long label increments

          RLATINC  = (RTOP  - RBOT)  / 10.
          RLONGINC = (RIGHT - RLEFT) / 10.

          CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  RLONGINC)
          CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   RLATINC)
 


          CALL PSETR ('CONTOUR_MIN_LEVEL', -50.0)
          CALL PSETR ('CONTOUR_MAX_LEVEL',  -0.1)
          CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLUE')
          CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
          CALL PCONT

          CALL PSETR ('CONTOUR_MIN_LEVEL',  -0.1)
          CALL PSETR ('CONTOUR_MAX_LEVEL',   0.1)
          CALL PSETC ('CONTOUR_LINE_COLOUR',      'BLACK')
          CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BLACK')
          CALL PCONT

          CALL PSETR ('CONTOUR_MIN_LEVEL',   0.1)
          CALL PSETR ('CONTOUR_MAX_LEVEL',   50.0)
          CALL PSETC ('CONTOUR_LINE_COLOUR',      'RED')
          CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'RED')
          CALL PCONT


C         Draw contours and coastlines

          CALL PCOAST


C         Plot the title

          WRITE(TITLE,'(A,4F6.1)') '(L,R,T,B): ',
     X          RLEFT, RIGHT, RTOP, RBOT
          CALL PSETC ('TEXT_LINE_2', TITLE)

          CALL PTEXT

      RETURN
      END






#include "parse_command_line.h"
