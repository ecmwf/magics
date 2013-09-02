      PROGRAM CONTDOTSHADETEST1

C     A test of Magics++ dot shading, using lots of different areas


      PARAMETER (NLEV=11)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /480, 490., 500., 510., 520., 530.,
     +                 540., 550., 560., 570., 580./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_cellshade_test1')



C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB



C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
       


C     Set up the text attributes

      CALL PSETI ('TEXT_LINE_COUNT', 3)


C     Define the contour attributes

      CALL PSETC  ('LEGEND',                       'ON')
      CALL PSETC  ('CONTOUR_SHADE',                'ON')
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE',      'CELL_SHADING')
c      CALL PSETR ('CONTOUR_SHADE_CELL_RESOLUTION', 1.0)
c      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
c      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      

      



C     Start plotting - one page per geographical area.

C                        LEFT , RIGHT, TOP , BOTTOM, NEWPAGE?
      CALL PSETC ('TEXT_LINE_3', 'Whole Globe')
      CALL CONTOUR_TEST (-180.,  180.,  90.,  -90.,   0)

      CALL PSETC ('TEXT_LINE_3', 'Top-left corner')
      CALL CONTOUR_TEST (-180., -140.,  90.,   60.,   1)

      CALL PSETC ('TEXT_LINE_3', 'Top-right corner')
      CALL CONTOUR_TEST ( 140.,  180.,  90.,   60.,   1)

      CALL PSETC ('TEXT_LINE_3', 'Bottom-left corner')
      CALL CONTOUR_TEST (-180., -140., -60.,  -90.,   1)

      CALL PSETC ('TEXT_LINE_3', 'Bottom-right corner')
      CALL CONTOUR_TEST ( 140.,  180., -60.,  -90.,   1)

      CALL PSETC ('TEXT_LINE_3', 'Middle section')
      CALL CONTOUR_TEST ( -30.,  70.,   50.,   30.,   1)

      CALL PSETC ('TEXT_LINE_3', 'Inside the middle')
      CALL CONTOUR_TEST ( -20.,   0.,   20.,   10.,   1)

      CALL PSETC ('TEXT_LINE_3', 'Inside an enclosed blob')
      CALL CONTOUR_TEST (  60.,  80.,   76.,   70.,   1)

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
 



C         Draw contours and coastlines

          CALL PCONT
          CALL PCOAST


C         Plot the title

          WRITE(TITLE,'(A,4F6.1)') '(L,R,T,B): ',
     X          RLEFT, RIGHT, RTOP, RBOT
          CALL PSETC ('TEXT_LINE_2', TITLE)

          CALL PTEXT

      RETURN
      END






#include "parse_command_line.h"
