      PROGRAM MULTIPLEPROJECTIONS

C     This program demonstrates MAGICS ability to between
C     different projections within the same program.


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('multiple_projections')


      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')

C     Load our data

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB


C     Define the contour     

      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
 

C     Set up the coastline

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Test the projections - one per page

      CALL PLOT_WITH_PROJCTION ('POLAR_STEREOGRAPHIC',  0)
      CALL PLOT_WITH_PROJCTION ('CYLINDRICAL',          1)
      CALL PLOT_WITH_PROJCTION ('POLAR_STEREOGRAPHIC',  1)
      CALL PLOT_WITH_PROJCTION ('CYLINDRICAL',          1)
      CALL PLOT_WITH_PROJCTION ('MERCATOR',             1)
 
 
      CALL PCLOSE
      STOP
      END






      SUBROUTINE PLOT_WITH_PROJCTION (CPROJ, INEW)
      
          CHARACTER*80  TITLE        
          CHARACTER*(*) CPROJ


          CALL PSETC ('SUBPAGE_MAP_PROJECTION', CPROJ)

C         Optionally start a new page

          IF (INEW == 1) THEN
            CALL PNEW  ('SUPER_PAGE')
          END IF


C         Set the projection for this page

          CALL PSETC ('SUBPAGE_MAP_PROJECTION', CPROJ)


C         Plot the coastline

          CALL PCOAST


C         Plot the data

          CALL PCONT



C         Plot the title

          WRITE (TITLE,'(A,A)'), 'Projection: ', CPROJ
          CALL PSETC ('TEXT_LINE_1', TITLE)
          CALL PTEXT

      RETURN
      END






#include "parse_command_line.h"

