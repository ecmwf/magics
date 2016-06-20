C (C) Copyright 1996-2016 ECMWF.
C 
C This software is licensed under the terms of the Apache Licence Version 2.0
C which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation nor
C does it submit to any jurisdiction.
C

      PROGRAM LAYOUTMANUAL

      CALL POPEN
      CALL PARSE_COMMAND_LINE ('layout_manual_ex')


C     Define the superpage size to be 29.7 x 21.0 cm

      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 29.7)
      CALL PSETR ('SUPER_PAGE_X_LENGTH', 21.0)


C     The page size within the superpage will be half the superpage's
C     height, but its full width.

      CALL PSETR ('PAGE_Y_LENGTH',       14.85)
      CALL PSETR ('PAGE_X_LENGTH',       21.0)

      CALL PSETC ('PAGE_ID_LINE_USER_TEXT', 'layout_manual_ex')
      CALL PSETC ('MAP_COASTLINE_COLOUR',   'OLIVE')
      CALL PSETC ('MAP_GRID_COLOUR',        'OLIVE')


C     -----------------
C     FIRST SUPER-PAGE.
C     -----------------

      CALL PSETC ('TEXT_LINE_1','Plot Layout Example')
      CALL PCOAST
      CALL PTEXT

      CALL PSETC ('TEXT_LINE_1',        'Page One')
      CALL PSETC ('TEXT_MODE',          'POSITIONAL')    
      CALL PSETC ('TEXT_BORDER',        'ON')    
      CALL PSETC ('TEXT_BOX_BLANKING',  'ON')    
      CALL PSETR ('TEXT_BOX_X_POSITION',  8.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION', 13.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',    5.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',    1.5)     
      CALL PTEXT

      CALL PSETC ('TEXT_LINE_1',        'Subpage One')
      CALL PSETR ('TEXT_BOX_X_POSITION', 9.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION', 8.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',   3.5)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',   1.0)     
      CALL PTEXT
      CALL PSETC ('SUPER_PAGE_FRAME','ON')

      CALL PNEW  ('PAGE')
      CALL PSETC ('SUBPAGE_FRAME',      'OFF')
      CALL PSETC ('PAGE_ID_LINE',       'OFF')
      CALL PSETC ('TEXT_LINE_1',        'Super-Page One')
      CALL PSETR ('TEXT_BOX_X_POSITION', 4.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION', 12.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',   13.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',   2.5)     
      CALL PTEXT      



C     --------------------------------------
C     SECOND SUPER-PAGE.
C     --------------------------------------

      CALL PNEW  ('SUPER_PAGE')


      CALL PSETC ('MAP_COASTLINE_COLOUR',   'TAN')
      CALL PSETC ('MAP_GRID_COLOUR',        'TAN')


C     PLOT FIRST PAGE, WHICH HAS ONE SUBPAGE

      CALL PSETC ('PAGE_ID_LINE', 'ON')
      CALL PSETC ('SUBPAGE_FRAME', 'ON')
      CALL PSETC ('TEXT_MODE','TITLE')    
      CALL PSETC ('TEXT_BORDER','OFF')    
      CALL PCOAST
      CALL PSETC ('TEXT_LINE_1','Plot Layout Example')
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Page One')
      CALL PSETC ('TEXT_MODE','POSITIONAL')    
      CALL PSETC ('TEXT_BOX_BLANKING','ON')    
      CALL PSETR ('TEXT_BOX_X_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',13.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',5.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.2)     
      CALL PSETC ('TEXT_BORDER','ON')    
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Subpage One')
      CALL PSETR ('TEXT_BOX_X_POSITION',9.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',3.5)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.0)     
      CALL PTEXT
      CALL PNEW  ('PAGE')
C
C     PLOT SECOND PAGE, WHICH HAS TWO SUBPAGES
C
      CALL PSETR ('SUBPAGE_X_LENGTH',8.25)
      CALL PSETR ('SUBPAGE_Y_LENGTH',8.25)
      CALL PSETR ('SUBPAGE_X_POSITION',1.5)
      CALL PSETR ('SUBPAGE_Y_POSITION',1.5)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',0.0)
      CALL PSETC ('TEXT_MODE','TITLE')    
      CALL PSETC ('TEXT_BORDER','OFF')    
      CALL PSETC ('TEXT_LINE_1','Plot Layout Example')
      CALL PCOAST
      CALL PTEXT
      CALL PNEW  ('SUBPAGE')
C
      CALL PSETR ('SUBPAGE_X_POSITION',11.5)
      CALL PSETR ('SUBPAGE_UPPER_RIGHT_LONGITUDE',180.0)
      CALL PSETR ('SUBPAGE_LOWER_LEFT_LONGITUDE',0.0)
      CALL PCOAST
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Page Two')
      CALL PSETC ('TEXT_MODE','POSITIONAL')    
      CALL PSETC ('TEXT_BORDER','ON')    
      CALL PSETC ('TEXT_BOX_BLANKING','ON')    
      CALL PSETR ('TEXT_BOX_X_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',11.3)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',5.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.2)     
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Super-Page Two')
      CALL PSETR ('TEXT_BOX_X_POSITION',4.0)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',12.8)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',13.0)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.9)     
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Subpage One')
      CALL PSETR ('TEXT_BOX_X_POSITION',3.9)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',3.5)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.0)     
      CALL PTEXT
      CALL PSETC ('TEXT_LINE_1','Subpage two')
      CALL PSETR ('TEXT_BOX_X_POSITION',13.2)   
      CALL PSETR ('TEXT_BOX_Y_POSITION',8.0)   
      CALL PSETR ('TEXT_BOX_X_LENGTH',3.5)     
      CALL PSETR ('TEXT_BOX_Y_LENGTH',1.0)     
      CALL PTEXT
C
C     CLOSE MAGICS
C
      CALL PCLOSE
C
      END














CC     This program demonstrates the manual layout facilities in MAGICS.
CC     We generate two plots on the same page.
C
C
CC     Open MAGICS and set the output device
C
C      CALL POPEN
C      CALL PARSE_COMMAND_LINE ('layout_manual_ex')
C
C
CC     Set up the main page dimensions
C
C      CALL PSETC ('LAYOUT',             'POSITIONAL')
C      CALL PSETR ('SUPER_PAGE_Y_LENGTH', 21.0)
C      CALL PSETR ('SUPER_PAGE_X_LENGTH', 29.7)
C
C
C
CC     Set up the coastline attributes
C
C      CALL PSETC ('MAP_COASTLINE',        'OFF')
C      CALL PSETC ('MAP_COASTLINE_COLOUR', 'GREY')
C      CALL PSETC ('MAP_GRID_COLOUR',      'GREY')     
C
C
CC     Pass the data to MAGICS
C
C      CALL PSETC ('GRIB_INPUT_TYPE','FILE')
C      CALL PSETC ('GRIB_INPUT_FILE_NAME','data/z500.grb')
C      CALL PGRIB
C
C
CC     Define the contour     
C
C      CALL PSETC  ('CONTOUR_LINE_COLOUR',      'BLUE')
C      CALL PSETC  ('CONTOUR_HIGHLIGHT_COLOUR', 'BLUE')
C
C
CC     Set up our generic text attributes
C
C      CALL PSETI ('TEXT_LINE_COUNT',1)
C      CALL PSETC ('TEXT_JUSTIFICATION','CENTRE')
C
C
CC     Page 1
C
C
C      CALL PSETR ('PAGE_X_LENGTH',        8.25)
C      CALL PSETR ('PAGE_Y_LENGTH',        8.25)
C      CALL PSETR ('PAGE_X_POSITION',      1.5)
C      CALL PSETR ('PAGE_Y_POSITION',      1.5)
C      CALL PSETC ('TEXT_LINE_1', 'Page 1')
C      CALL PTEXT
C      CALL PCONT
C      CALL PCOAST
C
C
CC     Page 2
C
C      CALL PNEW  ('PAGE')
C      CALL PSETR ('PAGE_X_POSITION', 11.5)
C
C      CALL PSETC ('TEXT_LINE_1', 'Page 2')
C      CALL PTEXT
C      CALL PCONT
C      CALL PCOAST
C
C
C
CC     Close
C
C      CALL PCLOSE
C
C      STOP
C      END
C

#include "parse_command_line.h"
