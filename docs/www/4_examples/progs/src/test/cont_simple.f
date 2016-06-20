C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONTSIMPLE

C     This program demonstrates magics contouring facilities.
C     Set to 'AUTOMATIC', Magics++ attempts to find the 'best'
C     contouring parameters for the trade-off between quality and speed. 


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PSETC ('PS_DEVICE',    'ps_a4')
      CALL PSETC ('PS_FILE_NAME', 'cont_simple.ps')


      CALL PSETR ('SUPER_PAGE_Y_LENGTH', (21.0 / 2.0))
      CALL PSETR ('SUPER_PAGE_X_LENGTH', (29.7 / 2.0))
      CALL PSETR ('PAGE_Y_LENGTH',       (21.0 / 2.0))
      CALL PSETR ('PAGE_X_LENGTH',       (29.7 / 2.0))



C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')


C     Pass the data to MAGICS
 
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('CONTOUR_METHOD',           'AKIMA760')
      CALL PSETR  ('AKIMA_RESOLUTION_X',        0.8)
      CALL PSETR  ('AKIMA_RESOLUTION_Y',        0.8)
      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PCONT
      

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
     x            'SWNE = -68.0, -135, 68, 135')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST



C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    -22.5)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   -45.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    22.5)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   45.0)

      CALL PSETC ('TEXT_LINE_1', 
     x            'SWNE = -22.5, -45.0, 22.5, 45.0')

      CALL PCONT
      CALL PTEXT
      CALL PCOAST

      CALL PCLOSE

      STOP
      END
