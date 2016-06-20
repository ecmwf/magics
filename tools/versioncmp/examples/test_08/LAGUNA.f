C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      SUBROUTINE LAGUNA

C    THIS ROUTINE IS TO HIDE THE VENITIAN LGOON WITH UNIFORM COLOUR

      PARAMETER( NPS=10, NPOL=2)

      REAL RX1(NPS), RY1(NPS)
      REAL RLAG(NPOL)

      DATA RX1/ 12.30, 12.40, 12.55, 12.60, 12.10, 12.10,
     .          12.25, 12.30, 12.30, 12.30/
      DATA RY1/ 45.32, 45.44, 45.48, 45.55, 45.55, 45.20,
     .          45.20, 45.20, 45.30, 45.32/

      PARAMETER( NPM=9)
      REAL RX2(NPM), RY2(NPM)
      DATA RX2/13.08, 13.15, 13.30, 13.38, 13.45, 13.50, 13.50,
     .         13.00,13.08/
      DATA RY2/45.66, 45.71, 45.72, 45.68, 45.68, 45.80, 45.85,
     .         45.85,45.66/

      DATA RLAG/1.,2./
C ......................................................................


ccc   RLAG(1) = 1.0
ccc   RLAG(2) = 2.0

      call PSETC   ('legend',                          'off')
      call PSET1R  ('polyline_input_longitudes',     RX1,NPS)
      call PSET1R  ('polyline_input_latitudes',      RY1,NPS)
      call PSET1R  ('polyline_input_values',          RLAG,1)
ccc   CALL PSETR   ('polyline_input_break_indicator',  -999.)

      CALL PSETC   ('polyline_line_colour',           'grey')
      CALL PSETC   ('polyline_shade',                   'ON')
      CALL PSETC   ('polyline_shade_level_selection_type',  'list')
      CALL PSET1R  ('polyline_level_list',             RLAG,2)
      CALL PSETC   ('polyline_shade_min_level_colour', 'grey')
      CALL PSETC   ('polyline_shade_max_level_colour', 'grey')
      CALL PLINE

ccc   CALL PSETC   ('polyline_line_colour',           'grey')
      call PSET1R  ('polyline_input_longitudes',     RX2,NPM)
      call PSET1R  ('polyline_input_latitudes',      RY2,NPM)
      call PSET1R  ('polyline_input_values',          RLAG,2)
c     CALL PSETC   ('polyline_shade_min_level_colour', 'grey')
c     CALL PSETC   ('polyline_shade_max_level_colour', 'grey')
      CALL PLINE
      call PSETC  ('legend',                             'on')

      RETURN
      END
