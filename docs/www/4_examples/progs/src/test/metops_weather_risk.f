C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM METOPS_WEATHER_RISK

c
c     open magics
      
      CALL POPEN
      CALL PARSE_COMMAND_LINE ('metops_weather_risk')
      

C   set up the page

      CALL PSETR ('SUPER_PAGE_X_LENGTH',   42.0)
      CALL PSETR ('SUPER_PAGE_Y_LENGTH',   29.7)
      CALL PSETR ('PAGE_X_LENGTH',         42.0)
      CALL PSETR ('PAGE_Y_LENGTH',         29.7)
      CALL PSETC ('SUBPAGE_FRAME_COLOUR', 'TAN')
      CALL PSETC ('PS_DEVICE',            'ps_a3')


C  set up and plot the coastlines

      CALL PSETC ('MAP_COASTLINE_COLOUR',         'RUST')
      CALL PSETC ('MAP_GRID',                     'ON')
      CALL PSETR ('MAP_GRID_LONGITUDE_INCREMENT',  10.)
      CALL PSETI ('MAP_LABEL_LATITUDE_FREQUENCY',  2)
      CALL PSETI ('MAP_LABEL_LONGITUDE_FREQUENCY', 2)
      CALL PSETR ('MAP_LABEL_HEIGHT',              0.3)
      CALL PSETC ('MAP_LABEL_QUALITY',            'HIGH')
      CALL PSETC ('MAP_GRID_COLOUR',              'GREY')
      CALL PSETC ('MAP_GRID_LINE_STYLE',          'DOT')
      CALL PSETC ('MAP_LABEL_COLOUR',             'BLACK')
c     CALL PSETC ('MAP_COASTLINE_LAND_SHADE',     'ON')
      CALL PSETC ('MAP_COASTLINE_LAND_SHADE_COLOUR', 'CREAM')
      CALL PCOAST
      
  
c  generate text for title

      CALL PSETC ('TEXT_LINE_1',  'EFI EXPERIMENTAL MAPS')
      CALL PSETI ('TEXT_LINE_COUNT',                 1)
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.5)
      CALL PSETC ('TEXT_JUSTIFICATION',             'CENTRE')
      CALL PTEXT


  
c  load the Z1000_em data

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/em.grb')
      CALL PGRIB


c  define the contour Z1000_em     

      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE',   'INTERVAL')
      CALL PSETR ('CONTOUR_INTERVAL',                4.)
      CALL PSETI ('CONTOUR_LINE_THICKNESS',          1)
      CALL PSETC ('CONTOUR_LINE_COLOUR',            'BLACK')
      CALL PSETI ('CONTOUR_HIGHLIGHT_THICKNESS',     3)
      CALL PSETC ('CONTOUR_LABEL_QUALITY',          'HIGH')
      CALL PSETR ('CONTOUR_LABEL_HEIGHT',            0.25)
      CALL PSETR ('CONTOUR_HILO_SUPPRESS_RADIUS',    40.)
      CALL PSETR ('CONTOUR_HILO_REDUCTION_RADIUS',   40.)
      CALL PSETC ('CONTOUR_LO_COLOUR',              'SKY')
      CALL PSETC ('CONTOUR_HI_COLOUR',              'RED')
      CALL PSETC ('CONTOUR_HILO_QUALITY',           'LOW')
      CALL PSETC ('CONTOUR_HILO',                   'ON')
      CALL PSETR ('CONTOUR_HILO_HEIGHT',             0.5)
c     CALL PSETC ('GRIB_SCALING_OF_DERIVED_FIELDS', 'ON')
      CALL PCONT


c  load the 2m temperature data 

      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/efi_2tm.grb')
      CALL PGRIB


C  contour for positive 2mT

c     CALL PSETC ('LEGEND',                         'ON')
      CALL PSETC ('CONTOUR',                        'OFF')
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE',   'INTERVAL')
      CALL PSETR ('CONTOUR_INTERVAL',                0.25) 
      CALL PSETR ('CONTOUR_REFERENCE_LEVEL',         0.5)
      CALL PSETC ('CONTOUR_SHADE',                  'ON')
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',         0.5)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',         1.)	
      CALL PSETR ('CONTOUR_MIN_LEVEL',               0.5)
      CALL PSETR ('CONTOUR_MAX_LEVEL',               1.)	
      CALL PSETC ('CONTOUR_SHADE_MIN_LEVEL_COLOUR', 'YELLOW')
      CALL PSETC ('CONTOUR_SHADE_MAX_LEVEL_COLOUR', 'ORANGE')
      CALL PSETC ('CONTOUR_SHADE_COLOUR_DIRECTION', 'CLOCKWISE')
      CALL PSETC ('CONTOUR_SHADE_METHOD',           'AREA_FILL')
      CALL PSETC ('CONTOUR_LABEL',                  'OFF')
      CALL PSETC ('CONTOUR_HILO',                   'OFF')
      CALL PCONT 


C  contour for negative 2mT

      CALL PSETR ('CONTOUR_INTERVAL',                0.25) 
      CALL PSETR ('CONTOUR_REFERENCE_LEVEL',        -0.5)
      CALL PSETC ('CONTOUR_SHADE',                  'ON')
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',        -1.)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',        -0.5)	
      CALL PSETR ('CONTOUR_MIN_LEVEL',              -1.)
      CALL PSETR ('CONTOUR_MAX_LEVEL',              -0.5)	
      CALL PSETC ('CONTOUR_SHADE_MIN_LEVEL_COLOUR', 'SKY')
      CALL PSETC ('CONTOUR_SHADE_MAX_LEVEL_COLOUR', 'RGB(0.8,0.9,0.9)')
      CALL PCONT 
	 


	
C  Load the wind data

      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/efi_fx.grb')
      CALL PGRIB


C  Define the contour for the wind events

      CALL PSETC ('CONTOUR',                      'OFF')
      CALL PSETC ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR ('CONTOUR_INTERVAL',              0.2) 
      CALL PSETC ('CONTOUR_SHADE',                'ON')
      CALL PSETR ('CONTOUR_REFERENCE_LEVEL',       0.6)
      CALL PSETC ('CONTOUR_SHADE_TECHNIQUE',      'MARKER') 
      CALL PSETR ('CONTOUR_SHADE_MIN_LEVEL',       0.6)
      CALL PSETR ('CONTOUR_SHADE_MAX_LEVEL',       1.0)	
      CALL PSETR ('CONTOUR_MIN_LEVEL',             0.6)
      CALL PSETR ('CONTOUR_MAX_LEVEL',             1.0)
      CALL PSET1C('CONTOUR_SHADE_COLOUR_TABLE',
     x           ['BLUISH_PURPLE', 'BLUISH_PURPLE'], 2)
      CALL PSET1R('CONTOUR_SHADE_HEIGHT_TABLE', [0.02, 0.2], 2)
      CALL PSET1I('CONTOUR_SHADE_MARKER_TABLE', [5, 17],     2)
      CALL PSETC ('CONTOUR_LABEL',                'OFF')
      CALL PSETC ('CONTOUR_HILO',                 'OFF')
      CALL PCONT 





C  Load the data for the total precipitation index

      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/efi_tp.grb')
      CALL PGRIB
      
C  Define and plot the contours for the total precipitation index

      CALL PSETC  ('CONTOUR',                 'OFF')
      CALL PSETC  ('CONTOUR_LEVEL_SELECTION_TYPE', 'INTERVAL')
      CALL PSETR  ('CONTOUR_INTERVAL',         0.2) 
      CALL PSETC  ('CONTOUR_SHADE',           'ON')
      CALL PSETR  ('CONTOUR_REFERENCE_LEVEL',  0.6)
      CALL PSETC  ('CONTOUR_SHADE_TECHNIQUE', 'MARKER') 
      CALL PSETR  ('CONTOUR_SHADE_MIN_LEVEL',  0.6)
      CALL PSETR  ('CONTOUR_SHADE_MAX_LEVEL',  1.0)	
      CALL PSETR  ('CONTOUR_MIN_LEVEL',        0.6)
      CALL PSETR  ('CONTOUR_MAX_LEVEL',        1.0)
      CALL PSET1C ('CONTOUR_SHADE_COLOUR_TABLE',
     x            ['KELLY_GREEN', 'KELLY_GREEN'] , 2)
      CALL PSET1R ('CONTOUR_SHADE_HEIGHT_TABLE', [0.02,0.2], 2)
      CALL PSET1I ('CONTOUR_SHADE_MARKER_TABLE', [6, 20], 2)
      CALL PSETC  ('CONTOUR_LABEL',           'OFF')
      CALL PSETC  ('CONTOUR_HILO',            'OFF')
      CALL PCONT


      CALL PCLOSE

      STOP
      END


#include "parse_command_line.h"
