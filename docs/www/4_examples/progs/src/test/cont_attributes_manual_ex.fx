      PROGRAM CONTATTRIBUTESMANUAL
      
C     This program demonstrates contour attributes in Magics++.
C     We use the manual layout facilities to create a set of plots
C     stacked vertically from the bottom of the page.


      REAL SUPER_W
      REAL SUPER_H
      REAL Y_INCREMENT
      INTEGER NUM_PLOTS_PER_PAGE



C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_attributes_manual_ex')


C     Set up the main page dimensions

      SUPER_W = 21.0
      SUPER_H = 29.7
      NUM_PLOTS_PER_PAGE = 4
      Y_INCREMENT = SUPER_H / (NUM_PLOTS_PER_PAGE*1.1)
      

      CALL PSETC ('LAYOUT',             'POSITIONAL')
      CALL PSETC ('PAGE_FRAME',         'ON')

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', SUPER_H)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', SUPER_W)

      CALL PSETR ('PAGE_X_LENGTH',   SUPER_W * 0.95)
      CALL PSETR ('PAGE_Y_LENGTH',   SUPER_H / (NUM_PLOTS_PER_PAGE*1.1))
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 0)


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE',        'ON')
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

      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Default Contours')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 1)
      CALL PSETC ('TEXT_LINE_1', 'Dot Contours')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LINE_STYLE',  'DOT')     
      CALL PSETC ('CONTOUR_LINE_COLOUR', 'BLUE')     
      CALL PCONT
      CALL PCOAST


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 2)
      CALL PSETC ('TEXT_LINE_1', 'Dash Contours')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LINE_STYLE',  'DASH')     
      CALL PSETC ('CONTOUR_LINE_COLOUR', 'BURGUNDY')     
      CALL PCONT
      CALL PCOAST


C     Page 4

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 3)
      CALL PSETC ('TEXT_LINE_1', 'Solid, Thick Contours')
      CALL PTEXT
      CALL PSETC ('CONTOUR_LINE_STYLE',      'SOLID')     
      CALL PSETC ('CONTOUR_LINE_COLOUR',     'GREEN')     
      CALL PSETI ('CONTOUR_LINE_THICKNESS',   4)     
      CALL PCONT
      CALL PCOAST



C     --------------------------------------------------
C     Start a new super page
C     Here we play with the contour highlight attributes
C     --------------------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 0)
      CALL PSETC ('CONTOUR_LINE_STYLE',     'SOLID')     
      CALL PSETC ('CONTOUR_LINE_COLOUR',    'BLUE')     
      CALL PSETI ('CONTOUR_LINE_THICKNESS',  1)     


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Default Highlights')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 1)
      CALL PSETC ('TEXT_LINE_1', 'Dot Highlights')
      CALL PTEXT
      CALL PSETC ('CONTOUR_HIGHLIGHT_STYLE',  'DOT')     
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'OLIVE')     
      CALL PCONT
      CALL PCOAST


C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 2)
      CALL PSETC ('TEXT_LINE_1', 'Dash, Frequent(2) Highlights')
      CALL PTEXT
      CALL PSETC ('CONTOUR_HIGHLIGHT_STYLE',  'DASH')     
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR', 'BURGUNDY')     
      CALL PSETI ('CONTOUR_HIGHLIGHT_FREQUENCY', 2)     
      CALL PCONT


C     Page 4

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 3)
      CALL PSETC ('TEXT_LINE_1', 'Solid, Thick Highlights / ev 6')
      CALL PTEXT
      CALL PSETC ('CONTOUR_HIGHLIGHT__STYLE',   'SOLID')     
      CALL PSETC ('CONTOUR_HIGHLIGHT_COLOUR',   'GREEN')     
      CALL PSETI ('CONTOUR_HIGHLIGHT_THICKNESS', 4)     
      CALL PSETI ('CONTOUR_HIGHLIGHT_FREQUENCY', 6)     
      CALL PCONT



C     -----------------------------------------
C     Start a new super page
C     Here we play with the high/low attributes
C     -----------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 0)
      CALL PSETC ('CONTOUR_LINE_STYLE',         'SOLID')     
      CALL PSETC ('CONTOUR_LINE_COLOUR',        'BLUE')     
      CALL PSETI ('CONTOUR_LINE_THICKNESS',      1)     
      CALL PSETC ('CONTOUR_HIGHLIGHT_STYLE',    'SOLID')     
      CALL PSETI ('CONTOUR_HIGHLIGHT_FREQUENCY', 4)     


C     Page 1

      CALL PSETC ('TEXT_LINE_1', 'Large Red HI, Green LO')
      CALL PTEXT
      CALL PSETC ('CONTOUR_HI_COLOUR',  'RED')     
      CALL PSETC ('CONTOUR_LO_COLOUR',  'GREEN')     
      CALL PSETR ('CONTOUR_HILO_HEIGHT',  0.6)     
      CALL PCONT


C     Page 2

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 1)
      CALL PSETC ('TEXT_LINE_1', 'HI LO Numbers')
      CALL PSETC ('CONTOUR_HILO_TYPE', 'NUMBER')
      CALL PSETR ('CONTOUR_HILO_HEIGHT',  0.3)     
      CALL PTEXT
      CALL PCONT



C     Page 3

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 2)
      CALL PSETC ('TEXT_LINE_1', 'HI LO Markers')
      CALL PSETC ('CONTOUR_HILO_MARKER', 'ON')
      CALL PTEXT
      CALL PCONT


C     Page 4

      CALL PNEW  ('PAGE')
      CALL PSETR ('PAGE_Y_POSITION', Y_INCREMENT * 3)
      CALL PSETC ('TEXT_LINE_1', 'Large Markers')
      CALL PSETR ('CONTOUR_HILO_MARKER_HEIGHT', 0.4)
      CALL PSETI ('CONTOUR_HILO_MARKER_INDEX',  2)
      CALL PSETC ('CONTOUR_HILO_MARKER_COLOUR', 'PURPLE')
      
      CALL PTEXT
      CALL PCONT


C     Close

      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"

