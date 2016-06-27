C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM COASTBOUNDARIES
C
C     No data is plotted, just the global coastline and boundaries
C
      CHARACTER*25 STATES
      DIMENSION    STATES(3)
      DATA         STATES /'Germany','United States of America',
     + 'Brazil'/
C
C     Open MAGICS and set the output device
C
      CALL POPEN
      CALL PARSE_COMMAND_LINE ('coast_boundaries')
C
C     Set up the coastline attributes
C
      CALL PSETC ('MAP_COASTLINE',        'ON')
      CALL PSETC ('MAP_COASTLINE_COLOUR', 'rust')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')
C
C     Rivers
C
      CALL PSETC ('MAP_RIVERS',       'ON')
      CALL PSETC ('MAP_RIVERS_COLOUR','blue')
C
C     Boundaries
C
      CALL PSETC ('MAP_BOUNDARIES',       'ON')
      CALL PSETC ('MAP_BOUNDARIES_COLOUR','green')
      CALL PSETC ('map_disputed_boundaries', 'on')
      CALL PSETC ('map_disputed_boundaries_colour', 'red')
      CALL PSETC ('map_administrative_boundaries', 'ON')
      CALL PSETC ('map_administrative_boundaries_colour','peach')
      CALL PSET1C('map_administrative_boundaries_countries_list',
     + STATES,3)
C
C     Plot the coastlines and then close
C
      CALL AREA_TEST (-180.,  180.,  90.,  -90.,   0)
C
C     Cities
C
      CALL PSETC ('map_cities', 'on')
      CALL PSETR ('map_cities_marker_height',3.0)
C
      CALL AREA_TEST ( -30.,   60.,  70.,   30.,   1)
      CALL AREA_TEST (  10.,   40.,  60.,   40.,   1)
      CALL AREA_TEST (   5.,   16.,  55.,   45.,   1)
      CALL AREA_TEST (-140.,  -60.,  50.,   25.,   1)
      CALL AREA_TEST ( -90.,  -60.,  30.,   10.,   1)
      CALL AREA_TEST ( -80.,  -60.,  50.,   40.,   1)
      CALL AREA_TEST ( -60.,  -40., -15.,  -35.,   1)
C
      CALL PCLOSE
C
      STOP
      END
C
C
C
      SUBROUTINE AREA_TEST (RLEFT, RIGHT, RTOP, RBOT, INEW)
C
C         Optionally start a new page
C
          IF (INEW == 1) THEN
            CALL PNEW  ('SUPER_PAGE')
          END IF
C
C         Set up the geographical area
C
          CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',    RBOT)
          CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',   RLEFT)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   RTOP)
          CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  RIGHT)
C
C         Calculate the best lat/long label increments
C
          RLATINC  = (RTOP  - RBOT)  / 10.
          RLONGINC = (RIGHT - RLEFT) / 10.
C
          CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  RLONGINC)
          CALL PSETR ('MAP_GRID_LATITUDE_INCREMENT',   RLATINC)
C 
C         Draw contours and coastlines
C
          CALL PCOAST
C
      RETURN
      END
C
C
#include "parse_command_line.h"
