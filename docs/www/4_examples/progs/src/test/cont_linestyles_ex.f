      PROGRAM CONTLINESTYLES

C     This program demonstrates the different line styles available
C     in Magics++.


C     Open MAGICS and set the output device

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('cont_linestyles_ex')


C     Set up the coastline attributes

      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     


C     Pass the data to MAGICS

      CALL PSETC ('GRIB_INPUT_TYPE',      'FILE')
      CALL PSETC ('GRIB_INPUT_FILE_NAME', 'data/z500.grb')
      CALL PSETC ('GRIB_SPECIFICATION',   'OFF')
c      CALL PSETC ('GRIB_SCALING',         'OFF')
      CALL PGRIB
      

C     Define the contour     

      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
      CALL PSETC  ('CONTOUR_LINE_STYLE',       'DASH')
      CALL PCONT


C     Set up and plot the title text

      CALL PSETC ('TEXT_LINE_1', 'Contours with dashed lines')
      CALL PSETI ('TEXT_LINE_COUNT',1)
      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
      CALL PTEXT
      CALL PCOAST



C     ------------------------------------
C     Redraw, using a different line style

      CALL PNEW   ('SUPER_PAGE')
      CALL PSETC  ('CONTOUR_LINE_STYLE',       'DOT')
      CALL PSETC  ('TEXT_LINE_1', 'Contours with dotted lines')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST



C     ------------------------------------
C     Redraw, using a different line style

      CALL PNEW   ('SUPER_PAGE')
      CALL PSETC  ('CONTOUR_LINE_STYLE',       'CHAIN_DASH')
      CALL PSETC  ('TEXT_LINE_1', 'Contours with chain-dash lines')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST



C     ------------------------------------
C     Redraw, using a different line style

      CALL PNEW   ('SUPER_PAGE')
      CALL PSETC  ('CONTOUR_LINE_STYLE',       'CHAIN_DOT')
      CALL PSETC  ('TEXT_LINE_1', 'Contours with chain-dot lines')
      CALL PTEXT
      CALL PCONT
      CALL PCOAST


      CALL PCLOSE

      STOP
      END



#include "parse_command_line.h"
