C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM CONTAKIMAEXAMPLE

C     This program is intended to illustrate AKIMA contouring
C     - to be used as an example plot.


      PARAMETER (NLEV=13)
      DIMENSION  RLEV (NLEV)
      DATA       RLEV /-20., 0., 10., 20., 30., 40.,
     +                 50., 60., 70., 80., 90., 100., 120./



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_akima_example')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR',         'RGB(0.4,0.4,0.4)') 
      CALL PSETC ('MAP_GRID_COLOUR',              'RGB(0.4,0.4,0.4)') 
      CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   5.0)
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  5.0)


C     New page, this time using a different area specification
C     Area specification (SOUTH, WEST, NORTH, EAST )

c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',     40.0)
c      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',    20.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',    60.0)
c      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',   50.0)


      CALL PSETC ('SUBPAGE_MAP_PROJECTION', 'POLAR_STEREOGRAPHIC')
      CALL PSETC ('SUBPAGE_MAP_AREA_DEFINITION', 'CENTRE')
      CALL PSETR ('SUBPAGE_MAP_CENTRE_LONGITUDE', 35.0)
      CALL PSETR ('SUBPAGE_MAP_CENTRE_LATITUDE',  50.0)
      CALL PSETC ('SUBPAGE_MAP_HEMISPHERE',      'NORTH')
      CALL PSETR ('SUBPAGE_MAP_SCALE',            10E6)



C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',     'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/rh850.grib')
      CALL PGRIB
      

C     Define the shared contour parameters

      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'LEVEL_LIST')
      CALL PSET1R ('CONTOUR_LEVEL_LIST',            RLEV, NLEV)            
      CALL PSETR  ('CONTOUR_AKIMA_X_RESOLUTION',        0.1)
      CALL PSETR  ('CONTOUR_AKIMA_Y_RESOLUTION',        0.1)
      CALL PSETI  ('CONTOUR_LABEL_FREQUENCY',   1)
      CALL PSETI  ('CONTOUR_LINE_THICKNESS',    3)
      CALL PSETC  ('CONTOUR_HILO',             'OFF')
      CALL PSETC  ('CONTOUR_HIGHLIGHT',        'OFF')
      

C     Set up the title text

C      CALL PSETC ('TEXT_LINE_1',
C     x            'LINEAR (blue) vs AKIMA760 (red) Contouring')
C
C      CALL PSETC ('CONTOUR_METHOD',           'LINEAR')
C      CALL PSETC ('CONTOUR_LINE_COLOUR',      'RGB(0.6,0.6,1.0)')
C      CALL PSETC ('CONTOUR_LABEL_COLOUR',     'RGB(0.3,0.3,1.0)')
C      CALL PCONT

      CALL PSETC ('TEXT_LINE_1',
     x 'Akima Contouring; Relative Humidity, 850hPa; 1st March 2005.')
      CALL PSETR  ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.7)

      CALL PSETC ('CONTOUR_METHOD',           'AKIMA760')
      CALL PSETC ('CONTOUR_LINE_COLOUR',      'RGB(1.0,0.2,0.2)')
      CALL PSETC ('CONTOUR_LABEL_COLOUR',     'RGB(1.0,0.1,0.1)')
      CALL PCONT

      CALL PTEXT
      CALL PCOAST



C     Close

      CALL PCLOSE

      STOP
      END


#include "parse_command_line.h"
