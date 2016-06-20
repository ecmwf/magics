C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM TEXTATTRIBUTES
      
C     This program demonstrates text plotting in Magics++.
C     We use the automatic layout facilities to create a set of plots
C     stacked vertically from the top of the page.


      REAL SUPER_W
      REAL SUPER_H
      INTEGER NUM_PLOTS_PER_PAGE
      REAL BOX_HEIGHT
      PARAMETER   (NFONTS=5)
      CHARACTER*22 CFONTS
      DIMENSION    CFONTS (NFONTS)
      DATA         CFONTS  /'Courier',
     x                      'Helvetica',		
     x                      'Arial',		
     x                      'Times',		
     x                      'Symbol'/		

      PARAMETER   (NSTYLES=4)
      CHARACTER*22 CSTYLES
      DIMENSION    CSTYLES (NSTYLES)
      DATA         CSTYLES  /'',
     x                       'Bold',		
     x                       'Italic',		
     x                       'BoldItalic'/		


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('text_ex')


C     Set up the main page dimensions

      SUPER_W = 21.0
      SUPER_H = 29.7
      NUM_PLOTS_PER_PAGE = 2


      CALL PSETC ('PS_DEVICE',          'ps_a4')
      CALL PSETC ('LAYOUT',             'AUTOMATIC')
      CALL PSETC ('PLOT_START',         'TOP')
      CALL PSETC ('PLOT_DIRECTION',     'VERTICAL')

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', SUPER_H)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', SUPER_W)

      CALL PSETR ('PAGE_Y_LENGTH', SUPER_H / (NUM_PLOTS_PER_PAGE*1.1))
      CALL PSETR ('PAGE_Y_GAP',    SUPER_H / 60.0)
      CALL PSETR ('PAGE_X_LENGTH', SUPER_W)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'OFF')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     



C     Define the geographical area for our first page

      CALL PSETR ('SUBPAGE_LOWER_LEFT_LATITUDE',   -20.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',  -100.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LATITUDE',   60.0)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',  100.0)


C     Pass the data to MAGICS
 
      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PGRIB


C     Set up our generic text attributes

      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')


C     ----- First superpage ------


C     Page 1

      CALL PSETI ('TEXT_LINE_COUNT', 4)
      CALL PSETC ('TEXT_LINE_1', 'User-defined title text')
      CALL PSETC ('TEXT_LINE_2', 'Text Line 2')
      CALL PSETC ('TEXT_LINE_3', 'Text Line 3')
      CALL PSETC ('TEXT_LINE_4', 'Text Line 4')
      CALL PTEXT
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETI ('TEXT_FIRST_LINE', 3)
      CALL PSETC ('TEXT_LINE_3', 'Text Line 3 (should be the first)')
      CALL PTEXT



C     ----- Start a new superpage ------

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETC ('SUBPAGE_FRAME', 'OFF')
      CALL PSETI ('TEXT_LINE_COUNT', 1)
      CALL PSETI ('TEXT_FIRST_LINE', 1)
      CALL PSETC ('PAGE_ID_LINE_LOGO_PLOT', 'OFF')


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Left Justified')
      CALL PSETC ('TEXT_BORDER', 'ON')
      CALL PSETC ('TEXT_BORDER_COLOUR', 'RED')
      CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
      CALL PTEXT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Right Justified')
      CALL PSETC ('TEXT_JUSTIFICATION','RIGHT')
      CALL PTEXT




C     ----- Start a new superpage ------

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETI ('TEXT_LINE_COUNT', 2)


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Text ref height: 0.4')
      CALL PSETC ('TEXT_LINE_2', 'Justification: CENTRE')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.4)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      CALL PTEXT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Text ref height: 0.2')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.2)
      CALL PTEXT



C     ----- Start a new superpage ------

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETI ('TEXT_LINE_COUNT', 2)


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Text ref height: 0.8')
      CALL PSETC ('TEXT_LINE_2', 'Justification: CENTRE')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.8)
      CALL PSETC ('TEXT_JUSTIFICATION', 'CENTRE')
      CALL PTEXT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETC ('TEXT_LINE_1', 'Text ref height: 2.0')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 2.0)
      CALL PTEXT



C     ----- Start a new superpage ------

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('PAGE_Y_LENGTH',  SUPER_H)
      CALL PSETC ('SUBPAGE_FRAME', 'ON')
      CALL PSETC ('TEXT_MODE',     'POSITIONAL')
      CALL PSETC ('TEXT_COLOUR',   'OLIVE')


C     Text box 1

      CALL PSETI ('TEXT_LINE_COUNT', 4)
      CALL PSETC ('TEXT_LINE_1', 'Positional Text 1')
      CALL PSETC ('TEXT_LINE_2', 'Default Position')
      CALL PSETC ('TEXT_LINE_3', 'Ref Height: 0.5')
      CALL PSETC ('TEXT_LINE_4', 'Border style: Default')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.5)
      CALL PTEXT



C     Text box 2

      CALL PSETC ('TEXT_LINE_1', 'Positional Text 2')
      CALL PSETC ('TEXT_LINE_2', 'X,Y: 10,15; W,H: 8,4')
      CALL PSETC ('TEXT_LINE_3', 'Ref Height: 0.5')
      CALL PSETC ('TEXT_LINE_4', 'Border style: DASH')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.5)
      CALL PSETC ('TEXT_BORDER_LINE_STYLE', 'DASH')
      CALL PSETC ('TEXT_BORDER_COLOUR', 'KELLY_GREEN')
      CALL PSETR ('TEXT_BOX_X_POSITION', 10.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION', 15.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    4.0)
      CALL PTEXT


C     Text box 3

      CALL PSETC ('TEXT_LINE_1', 'Positional Text 3')
      CALL PSETC ('TEXT_LINE_2', 'X,Y: 5,20; W,H: 8,3')
      CALL PSETC ('TEXT_LINE_3', 'Ref Height: 0.3')
      CALL PSETC ('TEXT_LINE_4', 'Border style: DOT, thick: 3')
      CALL PSETC ('TEXT_BORDER_LINE_STYLE', 'DOT')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.3)
      CALL PSETI ('TEXT_BORDER_THICKNESS', 3)
      CALL PSETC ('TEXT_BORDER_COLOUR', 'ORANGE')
      CALL PSETR ('TEXT_BOX_X_POSITION',  5.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION', 20.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    3.0)
      CALL PTEXT



C     Text box 4

      CALL PSETC ('TEXT_LINE_1', 'Positional Text 4')
      CALL PSETC ('TEXT_LINE_2', 'X,Y: 5,6; W,H: 8,3')
      CALL PSETC ('TEXT_LINE_3', 'Ref Height: 0.3')
      CALL PSETC ('TEXT_LINE_4', 'Border style: CHAIN_DASH')
      CALL PSETC ('TEXT_BORDER_LINE_STYLE', 'CHAIN_DASH')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.3)
      CALL PSETI ('TEXT_BORDER_THICKNESS', 1)
      CALL PSETC ('TEXT_BORDER_COLOUR', 'PURPLE')
      CALL PSETR ('TEXT_BOX_X_POSITION',  5.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',  6.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    3.0)
      CALL PCONT
      CALL PTEXT


C     Text box 5

      CALL PSETC ('TEXT_LINE_1', 'Positional Text 5')
      CALL PSETC ('TEXT_LINE_2', 'X,Y: 9,4.5; W,H: 8,2')
      CALL PSETC ('TEXT_LINE_3', 'Blanking ON')
      CALL PSETC ('TEXT_LINE_4', 'Border style: SOLID')
      CALL PSETC ('TEXT_BORDER_LINE_STYLE', 'SOLID')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.3)
      CALL PSETC ('TEXT_BORDER_COLOUR',  'SKY')
      CALL PSETR ('TEXT_BOX_X_POSITION',  9.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',  4.5)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    2.0)
      CALL PSETC ('TEXT_BOX_BLANKING',   'ON')
      CALL PTEXT



C     Text box 6

      CALL PSETC ('TEXT_LINE_1', 'Positional Text 6')
      CALL PSETC ('TEXT_LINE_2', 'X,Y: 2,23; W,H: 8,6')
      CALL PSETC ('TEXT_LINE_3', 'Ratios: 1.5, 1.0, 0.6, 2.0')
      CALL PSETC ('TEXT_LINE_4', 'Border style: SOLID')
      CALL PSETC ('TEXT_BORDER_LINE_STYLE', 'SOLID')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.4)
      CALL PSETC ('TEXT_BORDER_COLOUR',  'SKY')
      CALL PSETR ('TEXT_BOX_X_POSITION',  2.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',  23.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    6.0)
      CALL PSETC ('TEXT_BOX_BLANKING',   'ON')
      CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_1', 1.5)
      CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_2', 1.0)
      CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_3', 0.6)
      CALL PSETR ('TEXT_LINE_HEIGHT_RATIO_4', 2.0)
      CALL PTEXT




C     Text box 7

      CALL PSETC ('TEXT_LINE_1', 'Positional Text 7')
      CALL PSETC ('TEXT_LINE_2', 'X,Y: 11,23; W,H: 8,6')
      CALL PSETC ('TEXT_LINE_3', 'Space ratio: 3.0')
      CALL PSETC ('TEXT_LINE_4', 'Border style: SOLID')
      CALL PSETC ('TEXT_BORDER_LINE_STYLE', 'SOLID')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.5)
      CALL PSETC ('TEXT_BORDER_COLOUR',  'TAN')
      CALL PSETR ('TEXT_BOX_X_POSITION',  11.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',  23.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    10.0)
      CALL PSETC ('TEXT_BOX_BLANKING',   'ON')
      CALL PSETR ('TEXT_LINE_SPACE_RATIO', 3.0)
      CALL PTEXT




C     Text box 8

      CALL PSETC ('TEXT_COLOUR', 'GREY')
      CALL PSETC ('TEXT_LINE_1', 'Positional Text 8')
      CALL PSETC ('TEXT_LINE_2', 'X,Y: 1,11; W,H: 8,3')
      CALL PSETC ('TEXT_LINE_3', 'Justification: LEFT')
      CALL PSETC ('TEXT_LINE_4', 'Colour: @TEXT_COLOUR@')
      CALL PSETC ('TEXT_BORDER_LINE_STYLE', 'CHAIN_DASH')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.3)
      CALL PSETI ('TEXT_BORDER_THICKNESS', 1)
      CALL PSETC ('TEXT_BORDER_COLOUR', 'PURPLE')
      CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
      CALL PSETR ('TEXT_BOX_X_POSITION',  1.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',  11.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    3.0)
      CALL PSETR ('TEXT_LINE_SPACE_RATIO', 1.5)
      CALL PTEXT


C     Text box 9

      CALL PSETC ('TEXT_LINE_1', 'Positional Text 9')
      CALL PSETC ('TEXT_LINE_2', 'X,Y: 9.5,11; W,H: 8,3')
      CALL PSETC ('TEXT_LINE_3', 'Font: Times')
      CALL PSETC ('TEXT_LINE_4', 'Colour: @TEXT_COLOUR@')
      CALL PSETC ('TEXT_FONT',   'Times')
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.3)
      CALL PSETI ('TEXT_BORDER_THICKNESS', 1)
      CALL PSETC ('TEXT_BORDER_COLOUR', 'PURPLE')
      CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
      CALL PSETR ('TEXT_BOX_X_POSITION',  9.5)
      CALL PSETR ('TEXT_BOX_Y_POSITION',  11.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    3.0)
      CALL PTEXT




C     Text box 10

      CALL PSETC ('TEXT_LINE_1', 'Positional Text 10')
      CALL PSETC ('TEXT_LINE_2', 'X,Y: 1,15; W,H: 8,3')
      CALL PSETC ('TEXT_LINE_3', 'Font: XXX')
      CALL PSETC ('TEXT_LINE_4', 'Colour: @TEXT_COLOUR@')
      CALL PSETC ('TEXT_FONT',   'XXX')
      CALL PSETR ('TEXT_FONT_SIZE',   0.4)
      CALL PSETR ('TEXT_REFERENCE_CHARACTER_HEIGHT', 0.3)
      CALL PSETI ('TEXT_BORDER_THICKNESS', 1)
      CALL PSETC ('TEXT_BORDER_COLOUR', 'PURPLE')
      CALL PSETC ('TEXT_JUSTIFICATION', 'LEFT')
      CALL PSETR ('TEXT_BOX_X_POSITION',  1.0)
      CALL PSETR ('TEXT_BOX_Y_POSITION',  15.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    8.0)
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    3.0)
      CALL PTEXT



C     ----- Start a new superpage ------

      CALL PNEW  ('SUPER_PAGE')
      CALL PSETR ('PAGE_Y_LENGTH', SUPER_H)
      CALL PSETC ('SUBPAGE_FRAME', 'OFF')
      CALL PSETC ('TEXT_MODE',     'POSITIONAL')
      CALL PSETC ('TEXT_COLOUR',   'OLIVE')
    


      BOX_HEIGHT = SUPER_H / (NFONTS*NSTYLES*1.1)

      CALL PSETR ('TEXT_BOX_Y_LENGTH', BOX_HEIGHT)
      CALL PSETR ('TEXT_BOX_X_POSITION',  5.0)
      CALL PSETR ('TEXT_BOX_X_LENGTH',    12.0)
      CALL PSETI ('TEXT_LINE_COUNT',      1)
      CALL PSETC ('TEXT_BORDER_LINE_STYLE', 'SOLID')
      CALL PSETC ('TEXT_BORDER_COLOUR',     'RED')


      RPOS = SUPER_H - (BOX_HEIGHT * 1.05)

      DO I = 1, NFONTS
        DO J = 1, NSTYLES
          CALL PSETR ('TEXT_BOX_Y_POSITION', RPOS)
          CALL PSETC ('TEXT_FONT',       CFONTS(I))
          CALL PSETC ('TEXT_FONT_STYLE', CSTYLES(J))
          CALL PSETC ('TEXT_LINE_1', 'Font: ' // CFONTS(I) //
     +  ',' // CSTYLES(J))
          CALL PTEXT
          RPOS = RPOS - (BOX_HEIGHT * 1.05)
        END DO
      END DO



C     ----- Start a new superpage ------
C         We will test TEXT_QUALITY

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETR ('TEXT_BOX_Y_POSITION', 10.0)
      CALL PSETC ('TEXT_QUALITY', 'LOW')
      CALL PSETC ('TEXT_LINE_1', 'Quality: LOW')
      CALL PTEXT

      CALL PSETR ('TEXT_BOX_Y_POSITION', 12.0)
      CALL PSETC ('TEXT_QUALITY', 'MEDIUM')
      CALL PSETC ('TEXT_LINE_1', 'Quality: MEDIUM')
      CALL PTEXT

      CALL PSETR ('TEXT_BOX_Y_POSITION', 14.0)
      CALL PSETC ('TEXT_QUALITY', 'HIGH')
      CALL PSETC ('TEXT_LINE_1', 'Quality: HIGH')
      CALL PTEXT



C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

