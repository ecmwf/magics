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


      PARAMETER (NLEV=10) !z500
c      PARAMETER (NLEV=5)  !q1000
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /530., 540., 550., 560., 570., 575., 580.,
     x                 585., 588., 590./
c      DATA       RLEV /0., 0.005, 0.01, 0.015, 0.02/


C     Open MAGICS and set the output device

      CALL POPEN
     
      CALL PSETC ('PS_FILE_NAME', 'tt.ps')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
c     1 quarter = 90 x 180 degrees
c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -45.0)
c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -90.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    45.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   90.0)
c     europe = 40 x 40 degrees
c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     33.0)
c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -13.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    73.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   27.0)
c     england= 10x10 degrees
c       CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     49.0)
c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    -7.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    59.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',    3.0)

C     Pass the data to MAGICS
 
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', '../data/z500.grb')
      CALL PGRIB
      

C     Define the contour     


      CALL PSETC  ('CONTOUR_METHOD',               'AUTOMATIC')

      CALL PSETC  ('CONTOUR_LINE_COLOUR',          'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',            'OFF')
      CALL PSETC  ('CONTOUR_LABEL',            'OFF')
c      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
c      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)


      do i = 1, 1 
      
      CALL PCONT

C     Plot the coastlines

      CALL PCOAST
      CALL PTEXT
      
      call pnew("PAGE")
      end do
      




      CALL PCLOSE

      STOP
      END
