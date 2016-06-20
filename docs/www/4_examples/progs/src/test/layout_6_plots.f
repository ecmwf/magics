C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM LAYOUT6PLOTS
      
      REAL X1, X2
      REAL Y1, Y2, Y3


C     Page locations. We have a 2x3 grid of plots

      X1 =  0.0
      X2 = 10.0
      Y1 =  0.0
      Y2 =  9.0
      Y3 = 18.0



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('layout_6_plots')


C     Set up our page layout

      CALL PSETC ('LAYOUT', 'POSITIONAL')


      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 29.7)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 21.0)
      CALL PSETR ('PAGE_X_LENGTH',        8.0)
      CALL PSETR ('PAGE_Y_LENGTH',        8.0)



C     Pass the data to MAGICS
 
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB


C     Set up our common plotting attributes

      CALL PSETC ('CONTOUR_HILO',                    'OFF')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE',        'ON')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')




C     Plot each of the 6 pages

      CALL PSETR ('PAGE_X_POSITION', X1)
      CALL PSETR ('PAGE_Y_POSITION', Y1)
      CALL PCOAST
      CALL PCONT

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_X_POSITION', X2)
      CALL PSETR ('PAGE_Y_POSITION', Y1)
      CALL PCOAST
      CALL PCONT

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_X_POSITION', X1)
      CALL PSETR ('PAGE_Y_POSITION', Y2)
      CALL PCOAST
      CALL PCONT

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_X_POSITION', X2)
      CALL PSETR ('PAGE_Y_POSITION', Y2)
      CALL PCOAST
      CALL PCONT

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_X_POSITION', X1)
      CALL PSETR ('PAGE_Y_POSITION', Y3)
      CALL PCOAST
      CALL PCONT

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_X_POSITION', X2)
      CALL PSETR ('PAGE_Y_POSITION', Y3)
      CALL PCOAST
      CALL PCONT

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_X_POSITION', X3)
      CALL PSETR ('PAGE_Y_POSITION', Y3)
      CALL PCOAST
      CALL PCONT


C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

