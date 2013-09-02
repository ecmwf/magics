      PROGRAM COASTUK

C     This program is intended to test different resolution coastlines
C     under Magics++ in order to determine how to define the automatic
C     coastline resolution selection algorithm.


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('coast_uk')



C     Set up the coastline attributes

      CALL PSETC ('MAP_GRID_COLOUR',              'BLACK') 
      CALL PSETC ('MAP_GRID',   'OFF')
C      CALL PSETC ('MAP_COASTLINE',                'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR',         'BLACK')


C     First page, this time using the global area

C                       LEFT , RIGHT, TOP , BOTTOM, FACTOR
C      CALL COAST_TEST (-180.0, 180.0, 90.0, -90.0, 0.15, 0)
C      CALL COAST_TEST (-180.0, 180.0, 90.0, -90.0, 0.2, 0)



C     Now zoom in a bit

C                       LEFT , RIGHT, TOP , BOTTOM, FACTOR

c      CALL COAST_TEST (-180.0, 180.0, 90.0, -90.0,  '0.04', 0)



C     List of selected factors/areas

c      CALL COAST_TEST (-180.0, 180.0, 90.0, -90.0,  '0.1', 0)
c      CALL COAST_TEST (-160.0, 160.0, 80.0, -80.0,  '0.1', 0)
c      CALL COAST_TEST (-135.0, 135.0, 68.0, -68.0,  '0.1', 0)
c      CALL COAST_TEST (-90.0, 90.0, 45.0, -45.0, '0.1', 0)
c      CALL COAST_TEST (-70.0, 70.0, 65.0, -5.0, '0.1', 0)
c      CALL COAST_TEST (-60.0, 60.0, 55.0, 5.0, '0.1', 0)
c      CALL COAST_TEST (-45.0, 45.0, 75.0, 35.0, '0.08', 0)
c      CALL COAST_TEST (-35.0, 35.0, 65.0, 40.0, '0.08', 1)
c      CALL COAST_TEST (-30.0, 30.0, 65.0, 40.0, '0.04', 1)
c      CALL COAST_TEST (-12.0, 42.0, 75.0, 35.0, '0.04', 1)
c      CALL COAST_TEST (-12.0, 10.0, 65.0, 50.0, '0.04', 0)
c      CALL COAST_TEST (-15.0, 10.0, 70.0, 50.0, '0.04', 0)
c      CALL COAST_TEST (-11.0, 5.0, 60.0, 50.0, '0.1', 0)



C      Misc
      CALL COAST_TEST (-15.0, 4.9, 62.0, 48.0, '0.025', 0)



C     Close

      CALL PCLOSE

      STOP
      END



      SUBROUTINE COAST_TEST (RLEFT, RIGHT, RTOP, RBOT, FACTOR, INEW)
      
          CHARACTER*80  TITLE
          CHARACTER*32  COASTFILE
          CHARACTER*(*) FACTOR


C         Optionally start a new page

          IF (INEW == 1) THEN
            CALL PNEW  ('SUPER_PAGE')
          END IF


C         Set up the geographical area

          CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    RBOT)
          CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   RLEFT)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   RTOP)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  RIGHT)


C         Plot the coastlines - one full res, one scaled

          CALL PCOAST



      RETURN
      END



#include "parse_command_line.h"



