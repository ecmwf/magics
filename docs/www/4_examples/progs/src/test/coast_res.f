C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM COASTRES

C     This program is intended to test different resolution coastlines
C     under Magics++ in order to determine how to define the automatic
C     coastline resolution selection algorithm.


      PARAMETER (RESLO=1.0)
      PARAMETER (RESHI=0.2)

      PARAMETER (NLEV=2)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /580., 590./


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('coast_res')




C     Set up the paper size

c      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 21.0)
c      CALL PSETR ('SUPER_PAGE_X_LENGTH', 29.7)
c      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 10.5)
c      CALL PSETR ('SUPER_PAGE_X_LENGTH', 14.85)
c      CALL PSETR ('PAGE_Y_LENGTH', 10.5)
c      CALL PSETR ('PAGE_X_LENGTH', 14.85)



C     Set up the coastline attributes

c      CALL PSETC ('MAP_COASTLINE_PATH',      '/scratch/coast_0.1.nc')
c      CALL PSETC ('MAP_COASTLINE_PATH',           'coast_test.nc')
      CALL PSETC ('MAP_GRID_COLOUR',              'GREY') 
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   10.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  20.0)
      CALL PSETC ('MAP_COASTLINE',                'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR',         'BLACK')


      

C     Set up the title text

      CALL PSETI ('TEXT_LINE_COUNT',     2)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')




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
      CALL COAST_TEST (-180.0, 180.0, 90.0, -90.0,  '0.4',  0)

      CALL COAST_TEST (-160.0, 160.0, 80.0, -80.0, '0.075', 1)
      CALL COAST_TEST (-135.0, 135.0, 68.0, -68.0, '0.075', 1)
      CALL COAST_TEST (-90.0, 90.0, 45.0, -45.0, '0.075', 1)
      CALL COAST_TEST (-45.0, 45.0, 20.0, -20.0, '0.15', 1)
      CALL COAST_TEST (-45.0, 45.0, 70.0, 30.0, '0.15', 1)
      CALL COAST_TEST (-15.0, 45.0, 70.0, 35.0, '0.15', 1)
      CALL COAST_TEST (-15.0, 5.0, 60.0, 50.0, '0.025', 1)



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


C         We need to draw contours so that the automatic contour algorithm
C         can tell us the actual paper area

c          CALL PSETC  ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
c          CALL PGRIB
c          CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
c          CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)      




C         Plot the coastlines - one full res, one scaled

          COASTFILE = 'coast_' // FACTOR // '.nc'
c          CALL PSETC ('MAP_COASTLINE_PATH',  COASTFILE)
c          CALL PSETC ('MAP_COASTLINE_PATH',  'coast.nc')

c          CALL PSETC ('MAP_COASTLINE_COLOUR', 'BLACK')
c          CALL PCOAST
CCC          CALL PSETR ('MAP_COASTLINE_SIMPLIFICATION_FACTOR', FACTOR)
CCC          CALL PSETR ('MAP_COASTLINE_SIMPLIFICATION_FACTOR', 0.1)
C          CALL PSETC ('MAP_COASTLINE_COLOUR', 'RED')
          CALL PCOAST


C         Create and plot the title

          WRITE(TITLE,'(A,4F6.1,A,A)') '(L,R,T,B): ',
     X          RLEFT, RIGHT, RTOP, RBOT, '  Factor: ', FACTOR
          CALL PSETC ('TEXT_LINE_1', TITLE)

          WRITE(TITLE,'(A,2F6.1)') '(W,H): ',
     X          RIGHT - RLEFT, RTOP - RBOT
          CALL PSETC ('TEXT_LINE_2', TITLE)

          CALL PTEXT
c          CALL PCONT

      RETURN
      END



#include "parse_command_line.h"



