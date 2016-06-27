C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM POLYLINE_3


      PARAMETER (NPOLYS=3)
      DIMENSION  RVALS (NPOLYS)
      DATA       RVALS /10., 15., 20./

      PARAMETER (NPOINTS=9+NPOLYS-1)
      DIMENSION  RX (NPOINTS)
      DATA       RX / 0., 10., 8., -999., 
     +               20., 13., 5., -999.,
     +               30., 35., 40.  /
      DIMENSION  RY (NPOINTS)
      DATA       RY / 0.,  5., 14., -999., 
     +                3., 10.,  5., -999.,
     +               15., 20., 10.  /



      CALL POPEN
      CALL PARSE_COMMAND_LINE ('polyline_3')


c      CALL PSETC ('MAPGEN_INPUT_FILE_NAME', 
c     +           'data/tigge_histogram.mapgen')

c      CALL PSETC ('GEO_INPUT_FILE_NAME', 'data/2mt.geo')

      CALL PSET1R ('POLYLINE_INPUT_LONGITUDES', RX,    NPOINTS)
      CALL PSET1R ('POLYLINE_INPUT_LATITUDES',  RY,    NPOINTS)
      CALL PSET1R ('POLYLINE_INPUT_VALUES',     RVALS, NPOLYS)
      CALL PSETR  ('POLYLINE_INPUT_BERAK_INDICATOR', -999.)

      call psetc('polyline_shade', 'on')
      call psetc('polyline_shade_colour_method',    'calculate')
      call psetc('polyline_shade_max_level_colour', 'red')
      call psetc('polyline_shade_min_level_colour', 'blue')
      call psetc('polyline_shade_colour_direction', 'clockwise')

      CALL PSETC ('POLYLINE_LINE_COLOUR', 'RED')

      CALL PLINE
      CALL PCOAST



C     Close

      CALL PCLOSE

      STOP
      END




      SUBROUTINE DRAW_CIRCLE (RCENTREX, RCENTREY, RADIUS, COLOUR)
      
          CHARACTER*80  COLOUR
          REAL RX, RY
          REAL R

          PARAMETER (NPOLYS=1)
          DIMENSION  RVALS (NPOLYS)
          DATA       RVALS /1./

          PARAMETER (NPOINTS=9+NPOLYS-1)
          DIMENSION  RX (NPOINTS)
          DATA       RX / 0., 10., 8., -999., 
     +               20., 13., 5., -999.,
     +               30., 35., 40.  /
          DIMENSION  RY (NPOINTS)
          DATA       RY / 0.,  5., 14., -999., 
     +                3., 10.,  5., -999.,
     +               15., 20., 10.  /



C         Calculate the coordinates of N points around a circle


          DO 10 RX = 0, MAX_SYMBOLS

 10      CONTINUE

      RETURN
      END






#include "parse_command_line.h"



